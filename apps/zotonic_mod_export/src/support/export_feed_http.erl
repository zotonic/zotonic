%% @copyright 2026 Marc Worrell
%% @doc Conditional HTTP responses for resource feeds.
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

-module(export_feed_http).
-export([response/3, not_modified/5]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% Only controller_feed uses conditional responses. Its search is bounded to
%% 100 rows; bulk exports continue directly through the streaming encoder.
-spec response(Id, ContentType, Context) -> {term(), z:context()} when
    Id :: term(), ContentType :: term(), Context :: z:context().
response(Id, ContentType, Context) ->
    {ok, Rows} = export_feed_query:export_resource_data(#export_resource_data{id=Id, content_type=ContentType}, Context),
    Ids = [R || R <- Rows, m_rsc:is_visible(R, Context)],
    Metadata = [revision(R, Context) || R <- Ids],
    Root = case Id of
        undefined -> undefined;
        _ -> revision(Id, Context)
    end,
    Fingerprint = {
        ContentType,
        z_context:get_req_path(Context),
        z_context:language(Context),
        Root,
        Metadata
    },
    ETag = binary:encode_hex(crypto:hash(sha256, term_to_binary(Fingerprint))),
    #{<<"pagelen">> := PageLen} = z_context:get(feed_query, Context),
    respond(Id, Ids, ContentType, ETag, export_feed:next_page(Rows, PageLen, Context)).

revision(Id, Context) ->
    {Id, m_rsc:p(Id, <<"modified">>, Context), m_rsc:p(Id, <<"version">>, Context)}.

respond(Id, Ids, ContentType, ETag, Context) ->
    Key = {?MODULE, z_context:get_req_path(Context), ContentType,
        z_acl:user(Context), z_context:language(Context)},
    Now = calendar:universal_time(),
    {Modified, Stable, DateSafe} = case z_depcache:get(Key, Context) of
        {ok, {ETag, Previous, Safe}} -> {Previous, true, Safe};
        {ok, {_, Previous, _}} -> {Now, false, Previous < Now};
        _ -> {Now, false, true}
    end,
    z_depcache:set(Key, {ETag, Modified, DateSafe}, 86400, Context),
    Context1 = z_context:set_resp_header(<<"etag">>, <<"W/\"", ETag/binary, "\"">>, Context),
    Context2 = z_context:set_resp_header(<<"last-modified">>, cow_date:rfc1123(Modified), Context1),
    Context3 = z_context:set_resp_header(<<"cache-control">>, <<"private, no-cache">>, Context2),
    Match = z_context:get_req_header(<<"if-none-match">>, Context),
    Since = z_context:get_req_header(<<"if-modified-since">>, Context),
    case not_modified(Match, Since, ETag, Modified, Stable andalso DateSafe andalso Modified < Now) of
        true -> {{halt, 304}, Context3};
        false -> export_helper:stream(Id, ContentType, [{resource_ids, Ids}], Context3)
    end.

%% If-None-Match takes precedence, including a nonmatching or malformed value.
%% Date-only validation requires a previously observed, stable representation.
-spec not_modified(Match, Since, ETag, Modified, Stable) -> boolean() when
    Match :: binary() | undefined, Since :: binary() | undefined,
    ETag :: binary(), Modified :: calendar:datetime(), Stable :: boolean().
not_modified(undefined, undefined, _ETag, _Modified, _Stable) -> false;
not_modified(undefined, Since, _ETag, Modified, true) ->
    try
        cow_date:parse_date(Since)
    of
        Date -> Modified =< Date andalso Date =< calendar:universal_time()
    catch
        _:_ -> false
    end;
not_modified(undefined, _Since, _ETag, _Modified, false) -> false;
not_modified(Match, _Since, ETag, _Modified, _Stable) ->
    Quoted = <<"\"", ETag/binary, "\"">>,
    lists:any(
        fun(Value) ->
            case string:trim(Value) of
                <<"*">> -> true;
                Quoted -> true;
                <<"W/", Quoted/binary>> -> true;
                _ -> false
            end
        end,
        binary:split(Match, <<",">>, [global])).

