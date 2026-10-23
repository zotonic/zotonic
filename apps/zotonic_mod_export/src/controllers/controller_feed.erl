%% @copyright 2026 Marc Worrell
%% @doc Shorthand feeds for resources, saved queries and request-defined searches.
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

-module(controller_feed).
-moduledoc("
Serve `/feed/:type` and `/feed/:type/:id` with optional search arguments.
Types are atom, rss, jsonfeed and json (an alias for JSON Feed).

An id selects a visible saved query, a collection's members, or one resource.
Without an id, the request arguments define a normal mod_search query. Both
bare search keys and q-prefixed keys are supported. Page length defaults to 20
and is capped at 100. Client search options are not passed to the search engine.
").

-export([
    malformed_request/1,
    resource_exists/1,
    forbidden/1,
    content_types_provided/1,
    process/4,
    export_resource_data/2,
    export_resource_filename/2,
    export_resource_content_disposition/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

malformed_request(Context) ->
    Context1 = z_context:ensure_qs(Context),
    Type = request_arg(type, Context1),
    case content_type(Type) of
        undefined ->
            {true, Context1};
        Mime ->
            try
                RawId = request_arg(id, Context1),
                Id = m_rsc:rid(RawId, Context1),
                Query = request_query(z_context:get_q_all_noz(Context1)),
                {false, z_context:set([
                        {feed_id, Id},
                        {feed_has_id, RawId =/= undefined andalso RawId =/= <<>>},
                        {feed_query, Query},
                        {feed_content_type, Mime}
                    ], Context1)}
            catch
                error:badarg -> {true, Context1};
                throw:{error, _} -> {true, Context1}
            end
    end.

resource_exists(Context) ->
    Exists = case z_context:get(feed_has_id, Context) of
        true -> m_rsc:exists(z_context:get(feed_id, Context), Context);
        _ -> true
    end,
    {Exists, Context}.

forbidden(Context) ->
    Forbidden = case z_context:get(feed_id, Context) of
        undefined -> false;
        Id -> m_rsc:exists(Id, Context) andalso not m_rsc:is_visible(Id, Context)
    end,
    {Forbidden, Context}.

content_types_provided(Context) ->
    {[z_context:get(feed_content_type, Context)], Context}.

process(_Method, _AcceptedCT, ProvidedCT, Context) ->
    Id = z_context:get(feed_id, Context),
    Dispatch = z_context:get(zotonic_dispatch, Context),
    Context1 = z_context:set(export_module, ?MODULE, Context),
    Stream = export_encoder:stream(Id, ProvidedCT, [{dispatch, Dispatch}], Context1),
    {Stream, export_helper:set_filename(Id, ProvidedCT, Dispatch, Context1)}.

%% Keep caller-supplied search options out of the execution options. Search and
%% each encoder retain the current user's ACL context.
export_resource_data(#export_resource_data{id=Id}, Context) ->
    #{<<"q">> := Terms, <<"page">> := Page, <<"pagelen">> := PageLen} = z_context:get(feed_query, Context),
    AllTerms = selection(Id, Context) ++ Terms,
    Query0 = #{
        <<"q">> => AllTerms
    },
    % A query_id can supply its own sort when it is expanded by mod_search.
    Query = case has_term(<<"query_id">>, AllTerms) of
        true ->
            Query0;
        false ->
            z_search_props:default_terms(
                Query0,
                [#{<<"term">> => <<"sort">>, <<"value">> => <<"-publication_start">>}])
    end,
    #search_result{result=Ids} = z_search:search(
        <<"query">>, Query, Page, PageLen,
        #{is_count_rows => false, is_single_page => true},
        Context),
    {ok, Ids}.

export_resource_filename(#export_resource_filename{}, _Context) -> {ok, <<"feed">>}.
export_resource_content_disposition(#export_resource_content_disposition{}, _Context) -> {ok, <<"inline">>}.

request_arg(Key, Context) ->
    z_context:get(Key, Context, z_context:get_q(atom_to_binary(Key), Context)).

content_type(<<"atom">>) -> {<<"application">>, <<"atom+xml">>, []};
content_type(<<"rss">>) -> {<<"application">>, <<"rss+xml">>, []};
content_type(<<"jsonfeed">>) -> {<<"application">>, <<"feed+json">>, []};
content_type(<<"json">>) -> {<<"application">>, <<"feed+json">>, []};
content_type(_) -> undefined.

%% Accept both cat=article and Zotonic's qcat=article conventions. from_qargs
%% preserves repeated terms and binds subject/object filters to their predicate.
request_query(Args) ->
    SearchArgs = lists:filtermap(fun search_arg/1, Args),
    Query = z_search_props:from_qargs(SearchArgs),
    case has_term(<<"qargs">>, maps:get(<<"q">>, Query)) of
        true -> throw({error, recursive_qargs});
        false -> ok
    end,
    PageLen = case maps:get(<<"pagelen">>, Query) of
        undefined -> 20;
        N -> min(100, N)
    end,
    (maps:without([<<"options">>], Query))#{<<"pagelen">> => PageLen}.

has_term(Name, Terms) ->
    lists:any(
        fun
            (#{<<"term">> := Term}) -> Term =:= Name;
            (#{<<"terms">> := Nested}) -> has_term(Name, Nested);
            (_) -> false
        end,
        Terms).

search_arg({Key, _})
    when Key =:= <<"id">>; Key =:= <<"type">>;
         Key =:= <<"options">>; Key =:= <<"qoptions">> ->
    false;
search_arg({<<"query_id">>, Value}) ->
    {true, {<<"qquery_id">>, Value}};
search_arg({<<"q", _/binary>> = Key, Value}) ->
    {true, {Key, Value}};
search_arg({Key, Value}) when is_binary(Key) ->
    {true, {<<"q", Key/binary>>, Value}}.

selection(undefined, _Context) -> [];
selection(Id, Context) ->
    case {m_rsc:is_a(Id, query, Context), m_rsc:is_a(Id, collection, Context)} of
        {true, _} -> [#{<<"term">> => <<"query_id">>, <<"value">> => Id}];
        {false, true} -> [#{<<"term">> => <<"hassubject">>, <<"value">> => [Id, haspart]}];
        _ -> [#{<<"term">> => <<"rsc_id">>, <<"value">> => Id}]
    end.
