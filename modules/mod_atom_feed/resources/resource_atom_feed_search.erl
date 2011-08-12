%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% Date: 2009-12-06
%% @doc Serve an atom feed for a search query. Query arguments are like /api/search.

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

-module(resource_atom_feed_search).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").



-export([
    init/1,
    service_available/2,
    allowed_methods/2,
    encodings_provided/2,
	expires/2,
	content_types_provided/2,
	charsets_provided/2,
	provide_content/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).


init(DispatchArgs) -> 
    {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:set(DispatchArgs, Context),
    Context2 = z_context:ensure_qs(Context1),
    ?WM_REPLY(true, Context2).

    
allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.


charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.


encodings_provided(ReqData, Context) ->
    {[
        {"identity", fun(X) -> X end}, 
        {"gzip", fun(X) -> zlib:gzip(X) end}
    ], ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"application/atom+xml", provide_content}], ReqData, Context}.


expires(ReqData, State) ->
    RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData),
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), RD1, State}.


provide_content(ReqData, Context) ->
    Query0 = z_context:get_q_all(Context),
    try
        {FeedTitle, Query} = case proplists:get_value("feed_title", Query0) of
                                 undefined ->
                                     {"Latest updates", Query0};
                                 T ->
                                     {T, proplists:delete("feed_title", Query0)}
                             end,
        Q = search_query:parse_request_args(Query),
        Q1 = Q ++ [{sort, "-rsc.modified"}],
        F = fun() ->
                    S = z_search:search({'query', Q1}, Context),
                    Vars = [{ids, S#search_result.result},
                            {qtext, proplists:get_value(text, Q1)},
                            {feed_title, FeedTitle},
                            {updated, z_context:get(last_modified, Context)},
                            {site_url, z_context:abs_url("", Context)}
                           ],
                    {Content, _Context1} = z_template:render_to_iolist("atom_feed_search.tpl", Vars, Context),
                    Content
            end,
        Content = z_depcache:memo(F, {atom_feed_search, Q}, ?MAX_AGE, [], Context),
        {Content, ReqData, Context}

    catch
        _: {error, {unknown_query_term, E}} ->
            ReqData1 = wrq:set_resp_body("Unknown query term: " ++ E, ReqData),
            {{halt, 400}, ReqData1, Context};

        _: {case_clause, {error, {error, error, _, _E, _}}} ->
            ReqData1 = wrq:set_resp_body("Unknown error.", ReqData),
            {{halt, 400}, ReqData1, Context}
    end.

