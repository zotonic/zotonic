%% @copyright 2026 Marc Worrell
%% @doc Search selection and export callbacks for shorthand feeds.
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

-module(export_feed_query).

-export([
    default_id/1,
    request_query/1,
    export_resource_data/2,
    export_resource_filename/2,
    export_resource_content_disposition/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Resolve the site's default feed query, using the visitor's visibility.
-spec default_id(Context) -> integer() | undefined when Context :: z:context().
default_id(Context) ->
    case m_rsc:rid(query_feed, Context) of
        undefined -> undefined;
        Id ->
            case m_rsc:is_a(Id, query, Context) andalso m_rsc:is_visible(Id, Context) of
                true -> Id;
                false -> undefined
            end
    end.

%% Keep caller-supplied search options out of the execution options. Search and
%% each encoder retain the current user's ACL context.
export_resource_data(#export_resource_data{id=Id}, Context) ->
    #{<<"q">> := Terms, <<"page">> := Page, <<"pagelen">> := PageLen} = z_context:get(feed_query, Context),
    AllTerms = feed_selection(Id, Context) ++ Terms,
    Query0 = #{
        <<"q">> => AllTerms ++ [
                        #{
                            <<"term">> => <<"zsort">>,
                            <<"value">> => [<<"-publication_start">>, <<"-id">>]
                        }
                    ]
    },
    % A query_id can supply its own sort when it is expanded by mod_search.
    Query = case has_term(<<"query_id">>, AllTerms) of
        true ->
            Query0;
        false ->
            z_search_props:default_terms(
                Query0,
                [
                    #{
                        <<"term">> => <<"sort">>,
                        <<"value">> => <<"-publication_start">>
                    }
                ])
    end,
    #search_result{result=Ids} = z_search:search(
        <<"query">>, Query, Page, PageLen,
        #{ is_count_rows => false, is_single_page => true },
        Context),
    {ok, Ids}.

export_resource_filename(#export_resource_filename{}, _Context) -> {ok, <<"feed">>}.
export_resource_content_disposition(#export_resource_content_disposition{}, _Context) -> {ok, <<"inline">>}.

-spec request_query(Args) -> map() when Args :: list().
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
    Query1 = maps:without([<<"options">>], Query),
    Query1#{
        <<"pagelen">> => PageLen
    }.

search_arg({Key, _})
    when Key =:= <<"id">>;
         Key =:= <<"type">>;
         Key =:= <<"options">>;
         Key =:= <<"qoptions">> ->
    false;
search_arg({<<"query_id">>, Value}) ->
    {true, {<<"qquery_id">>, Value}};
search_arg({<<"q", _/binary>> = Key, Value}) ->
    {true, {Key, Value}};
search_arg({Key, Value}) when is_binary(Key) ->
    {true, {<<"q", Key/binary>>, Value}}.

has_term(Name, Terms) ->
    lists:any(
        fun
            (#{<<"term">> := Term}) -> Term =:= Name;
            (#{<<"terms">> := Nested}) -> has_term(Name, Nested);
            (_) -> false
        end,
        Terms).

feed_selection(undefined, Context) ->
    case z_context:get(is_default_feed, Context, false) of
        true ->
            [
                #{
                    <<"term">> => <<"cat">>,
                    <<"value">> => <<"article">>
                },
                #{
                    <<"term">> => <<"is_published">>,
                    <<"value">> => true
                }
            ];
        false ->
            []
    end;
feed_selection(Id, Context) ->
    selection(Id, Context).

selection(Id, Context) ->
    case {m_rsc:is_a(Id, query, Context), m_rsc:is_a(Id, collection, Context)} of
        {true, _} ->
            [
                #{
                    <<"term">> => <<"query_id">>,
                    <<"value">> => Id
                }
            ];
        {false, true} ->
            [
                #{
                    <<"term">> => <<"hassubject">>,
                    <<"value">> => [Id, haspart]
                }
            ];
        {false, false} ->
            [
                #{
                    <<"term">> => <<"rsc_id">>,
                    <<"value">> => Id
                }
            ]
    end.
