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
-export([queue/4, download_task/6]).

-define(MAX_MEDIA_BYTES, 20 * 1024 * 1024).
-define(MAX_RETRIES, 5).
-define(RETRY_SECONDS, 60).

%% @doc Queue after saving the article and its edges. A resource ID is the unique
%% task key, so reimports replace pending work instead of adding duplicate tasks.
-spec queue(FeedId, Id, Url, Context) -> ok | {error, term()} when
    FeedId :: integer(), Id :: integer(), Url :: binary(), Context :: z:context().
queue(FeedId, Id, Url, Context) ->
    case m_rsc:p(Id, feed_medium_url, Context) =:= Url andalso m_media:get(Id, Context) =/= undefined of
        true -> ok;
        false ->
            case m_rsc:update(Id, #{<<"feed_medium_pending_url">> => Url}, [no_touch], Context) of
                {ok, _} ->
                    Medium = m_media:get(Id, Context),
                    case z_pivot_rsc:insert_task(?MODULE, download_task, Id,
                            [FeedId, Id, Url, Medium, 0], Context) of
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
        {true, _} -> {error, admin_import_user};
        {_, false} -> {error, editor_disabled};
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
    andalso m_rsc:p(Id, feed_medium_pending_url, Context) =:= Url
    andalso m_media:get(Id, Context) =:= Medium.

download(Id, Url, Medium, Context) ->
    case z_acl:rsc_editable(Id, Context) of
        false -> {error, eacces};
        true ->
            case z_import_feed_http:get(Url, ?MAX_MEDIA_BYTES, Context) of
                {ok, {FinalUrl, _, _, Data}} ->
                    %% A user or another import might replace the file while downloading.
                    case pending(Id, Url, Medium, Context) of
                        false -> ok;
                        true ->
                            Path = maps:get(path, uri_string:parse(z_convert:to_binary(FinalUrl)), <<>>),
                            Upload = #upload{filename = filename:basename(Path), data = Data},
                            Props = #{<<"feed_medium_url">> => Url, <<"feed_medium_pending_url">> => undefined},
                            case m_media:replace_file(Upload, Id, Props, [is_import, no_touch], Context) of
                                {ok, _} -> ok;
                                {error, _} = Error -> Error
                            end
                    end;
                {error, _} = Error -> Error
            end
    end.

result(ok, FeedId, Id, Url, _Medium, _Retry, Context) ->
    log(FeedId, Id, Url, <<"ok">>, undefined, Context);
result({error, Reason}, FeedId, Id, Url, Medium, Retry, Context)
    when Retry < ?MAX_RETRIES, Reason =/= eacces ->
    log(FeedId, Id, Url, <<"retry">>, Reason, Context),
    {delay, ?RETRY_SECONDS * (1 bsl Retry), [FeedId, Id, Url, Medium, Retry + 1]};
result({error, Reason}, FeedId, Id, Url, _Medium, _Retry, Context) ->
    log(FeedId, Id, Url, <<"error">>, Reason, Context).

log(FeedId, Id, Url, Status, Reason, Context) ->
    Message = case Reason of
        undefined -> undefined;
        _ -> m_import_feed:error_message(Reason, Context)
    end,
    m_import_feed_log:write(FeedId, #{event => <<"media">>, status => Status,
        uri => Url, rsc_id => Id, user_id => z_acl:user(Context),
        category_id => m_rsc:p(Id, category_id, Context),
        content_group_id => m_rsc:p(Id, content_group_id, Context), message => Message}, Context).
