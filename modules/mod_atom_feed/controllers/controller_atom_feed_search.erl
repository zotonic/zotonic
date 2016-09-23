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

-module(controller_atom_feed_search).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    allowed_methods/1,
    content_encodings_provided/1,
	expires/1,
	content_types_provided/1,
	charsets_provided/1,
	provide_content/1
]).

-include_lib("include/zotonic.hrl").

%% Let cached versions expire in an hour.
-define(MAX_AGE, 3600).


  
allowed_methods(Context) ->
    {[<<"HEAD">>, <<"GET">>], Context}.


charsets_provided(Context) ->
    {[<<"utf-8">>], Context}.


content_encodings_provided(Context) ->
    {[<<"identity">>, <<"gzip">>], Context}.


content_types_provided(Context) ->
    {[{<<"application/atom+xml">>, provide_content}], Context}.


expires(Context) ->
    Context1 = z_context:set_resp_header(<<"cache-control">>, <<"public, max-age=", (integer_to_binary(?MAX_AGE))/binary>>, Context),
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), Context1}.


provide_content(Context) ->
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
        Content1 = cowmachine_req:encode_content(Content, Context),
        {Content1, Context}

    catch
        _: {error, {unknown_query_term, E}} ->
            Context1 = cowmachine_req:set_resp_body(
                            cowmachine_req:encode_content("Unknown query term: " ++ E, Context),
                            Context),
            {{halt, 400}, Context1};

        _: {case_clause, {error, {error, error, _, _E, _}}} ->
            Context1 = cowmachine_req:set_resp_body(
                            cowmachine_req:encode_content("Unknown error.", Context),
                            Context),
            {{halt, 400}, Context1}
    end.

