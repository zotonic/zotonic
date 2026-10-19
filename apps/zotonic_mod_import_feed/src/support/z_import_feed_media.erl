%% @copyright 2026 Marc Worrell
%% @doc Persistent media downloads for feed resource placeholders.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_import_feed_media).

-include_lib("zotonic_core/include/zotonic.hrl").
-export([queue/4, download_task/6, resolve/2, identity/1, fetch/2]).

-define(MAX_MEDIA_BYTES, 20 * 1024 * 1024).
-define(MAX_RETRIES, 16).
-define(RETRY_SECONDS, 60).
-define(MAX_RETRY_SECONDS, 12 * 60 * 60).

%% @doc Media may be embedded data; feed and article URLs remain HTTP-only.
-spec resolve(binary(), binary()) -> binary().
resolve(<<"data:", _/binary>> = Url, _Base) -> Url;
resolve(Url, Base) -> z_import_feed_http:resolve(Url, Base).

%% @doc Keep embedded payloads out of indexed resource URIs and log records.
%% The original URL is retained only as the source for the queued media task.
-spec identity(binary()) -> binary().
identity(<<"data:", _/binary>> = Url) ->
    <<"urn:sha256:", (binary:encode_hex(crypto:hash(sha256, Url)))/binary>>;
identity(Url) -> Url.

%% @doc Reuse stdlib data URL decoding, with the same size bound as HTTP media.
%% Percent encoding needs at most three source bytes per decoded byte.
-spec fetch(binary(), z:context() | undefined) -> {ok, tuple()} | {error, term()}.
fetch(<<"data:", _/binary>> = Url, _Context) when byte_size(Url) > 3 * ?MAX_MEDIA_BYTES ->
    {error, response_too_large};
fetch(<<"data:", _/binary>> = Url, _Context) ->
    try z_url_fetch:fetch_partial(Url, [{max_length, ?MAX_MEDIA_BYTES}]) of
        {ok, {_, _, Length, Data}} when Length >= ?MAX_MEDIA_BYTES; byte_size(Data) >= ?MAX_MEDIA_BYTES ->
            {error, response_too_large};
        Result -> Result
    catch
        _:_ -> {error, invalid_data_url}
    end;
fetch(Url, Context) -> z_import_feed_http:get(Url, ?MAX_MEDIA_BYTES, Context).

%% @doc Queue after saving the article and its edges. A resource ID is the unique
%% task key, so reimports replace pending work instead of adding duplicate tasks.
-spec queue(FeedId, Id, Url, Context) -> ok | {error, term()} when
    FeedId :: integer(), Id :: integer(), Url :: binary(), Context :: z:context().
queue(FeedId, Id, Url, Context) ->
    case m_rsc:p(Id, feed_medium_url, Context) =:= identity(Url) andalso m_media:get(Id, Context) =/= undefined of
        true -> ok;
        false ->
            case m_rsc:update(Id, #{<<"feed_medium_pending_url">> => identity(Url)}, [no_touch], Context) of
                {ok, _} ->
                    Medium = m_media:get(Id, Context),
                    case z_pivot_rsc:insert_task(?MODULE, download_task, Id, [FeedId, Id, Url, Medium, 0], Context) of
                        {ok, _} -> log(FeedId, Id, Url, <<"queued">>, undefined, Context);
                        {error, _} = Error -> Error
                    end;
                {error, _} = Error -> Error
            end
    end.

%% @doc Recheck the feed's current account, permissions and pending URL on every
%% attempt. Never download in a transaction or overwrite a locally replaced file.
-spec download_task(FeedId, Id, Url, Medium, Retry, Context) -> z_pivot_rsc:task_return() when
    FeedId :: integer(), Id :: integer(), Url :: binary(),
    Medium :: map() | undefined, Retry :: non_neg_integer(), Context :: z:context().
download_task(FeedId, Id, Url, Medium, Retry, Context) ->
    case z_db:qmap("select * from import_feed where id=$1", [FeedId], [{keys, atom}], Context) of
        {ok, []} -> ok;
        {ok, [Row]} ->
            Feed = m_import_feed:decode(Row),
            case import_context(Feed, Context) of
                {ok, ImportContext} ->
                    case pending(Id, Url, Medium, ImportContext) of
                        false -> ok;
                        true ->
                            log(FeedId, Id, Url, <<"started">>, undefined, ImportContext),
                            Result = download(Id, Url, Medium, ImportContext),
                            result(Result, FeedId, Id, Url, Medium, Retry, ImportContext)
                    end;
                {error, Reason} ->
                    log(FeedId, Id, Url, <<"error">>, Reason, Context)
            end;
        {error, Reason} ->
            result({error, Reason}, FeedId, Id, Url, Medium, Retry, Context)
    end.

import_context(#{user_id := User} = Feed, Context) when is_integer(User) ->
    case {m_import_feed:is_admin_user(User, Context), z_auth:is_enabled(User, Context)} of
        {true, _} ->
            {error, admin_import_user};
        {_, false} ->
            {error, editor_disabled};
        {false, true} ->
            ImportContext = z_acl:logon(User, #{user_groups => maps:get(user_groups, Feed)}, z_context:new(Context)),
            case z_acl:is_read_only(ImportContext) of
                true -> {error, eacces};
                false -> {ok, z_import_feed_http:context(Feed, ImportContext)}
            end
    end;
import_context(_, _) -> {error, missing_editor}.

pending(Id, Url, Medium, Context) ->
    m_rsc:exists(Id, Context)
    andalso m_rsc:p(Id, feed_medium_pending_url, Context) =:= identity(Url)
    andalso m_media:get(Id, Context) =:= Medium.

download(Id, Url, Medium, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        false -> {error, eacces};
        true ->
            case fetch(Url, Context) of
                {ok, {FinalUrl, Headers, _, Data}} ->
                    %% A user or another import might replace the file while downloading.
                    case pending(Id, Url, Medium, Context) of
                        false -> ok;
                        true ->
                            Upload = #upload{
                                filename = media_filename(Url, FinalUrl, Headers),
                                data = Data
                            },
                            Props = #{
                                <<"feed_medium_url">> => identity(Url),
                                <<"feed_medium_pending_url">> => undefined
                            },
                            case m_media:replace_file(Upload, Id, Props, [is_import, no_touch], Context) of
                                {ok, _} -> ok;
                                {error, _} = Error -> Error
                            end
                    end;
                {error, _} = Error -> Error
            end
    end.

media_filename(<<"data:", _/binary>>, _FinalUrl, Headers) ->
    Mime = proplists:get_value("content-type", Headers, "application/octet-stream"),
    <<"embedded", (z_media_identify:extension(Mime))/binary>>;
media_filename(_, FinalUrl, _) ->
    Path = maps:get(path, uri_string:parse(z_convert:to_binary(FinalUrl)), <<>>),
    filename:basename(Path).

result(ok, FeedId, Id, Url, _Medium, _Retry, Context) ->
    log(FeedId, Id, Url, <<"ok">>, undefined, Context);
result({error, Reason}, FeedId, Id, Url, Medium, Retry, Context)
    when
        Retry < ?MAX_RETRIES,
        Reason =/= eacces ->
    log(FeedId, Id, Url, <<"retry">>, Reason, Context),
    {delay,
        max(min(?MAX_RETRY_SECONDS, ?RETRY_SECONDS * (1 bsl Retry)),
            z_import_feed_http:error_delay(Reason)),
        [FeedId, Id, Url, Medium, Retry + 1]};
result({error, Reason}, FeedId, Id, Url, _Medium, _Retry, Context) ->
    log(FeedId, Id, Url, <<"error">>, Reason, Context).

log(FeedId, Id, Url, Status, Reason, Context) ->
    Message = case Reason of
        undefined -> undefined;
        _ -> m_import_feed:error_message(Reason, Context)
    end,
    m_import_feed_log:write(
        FeedId,
        #{
            event => <<"media">>,
            status => Status,
            uri => identity(Url),
            rsc_id => Id,
            user_id => z_acl:user(Context),
            category_id => m_rsc:p(Id, category_id, Context),
            content_group_id => m_rsc:p(Id, content_group_id, Context), message => Message
        },
        Context).
