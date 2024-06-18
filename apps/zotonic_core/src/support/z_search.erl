%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2024 Marc Worrell
%% @doc Search the database, interfaces to specific search routines.
%% @end

%% Copyright 2009-2024 Marc Worrell
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

-module(z_search).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/5,
    search/6,

    search/2,
    search/3,
    search_pager/3,
    search_pager/4,
    search_result/3,
    query_/2,

    lookup_qarg_value/2,
    lookup_qarg_value/3,

    lookup_qarg/2,
    lookup_qarg/3,

    map_to_options/1,
    props_to_map/1,
    reformat_sql_query/3,
    concat_sql_query/2,

    default_pagelen/1
]).

% The tuple format is deprecated. Use separate binary search term with a map.
-type search_query():: {atom(), list()}
                     | {binary(), map()|undefined}.
-type search_offset() :: Limit :: pos_integer()
                       | {Offset :: pos_integer(), Limit :: pos_integer()}.
-type search_options() :: #{
        properties => list(binary()) | boolean(),
        is_count_rows => boolean()
    }.

-export_type([
    search_query/0,
    search_offset/0,
    search_options/0
    ]).

-include_lib("zotonic.hrl").

-define(SEARCH_ALL_LIMIT, 30000).
-define(MIN_LOOKAHEAD, 200).

%% @doc Perform a named search with arguments.
-spec search(Name, Args, Page, PageLen, Context) -> Result when
    Name :: binary(),
    Args :: map() | proplists:proplist() | undefined,
    Page :: pos_integer() | undefined,
    PageLen :: pos_integer() | undefined,
    Context :: z:context(),
    Result :: #search_result{}.
search(Name, #{ <<"options">> := Options } = Args, Page, PageLen, Context) ->
    search(Name, Args, Page, PageLen, Options, Context);
search(Name, Args, Page, PageLen, Context) ->
    search(Name, Args, Page, PageLen, #{}, Context).

%% @doc Perform a named search with arguments.
-spec search(Name, Args, Page, PageLen, Options, Context) -> Result when
    Name :: binary(),
    Args :: map() | proplists:proplist() | undefined,
    Page :: pos_integer() | undefined,
    PageLen :: pos_integer() | undefined,
    Options :: map(),
    Context :: z:context(),
    Result :: #search_result{}.
search(Name, undefined, Page, PageLen, Options, Context) ->
    search(Name, #{}, Page, PageLen, Options, Context);
search(Name, Args, undefined, PageLen, Options, Context) ->
    search(Name, Args, 1, PageLen, Options, Context);
search(Name, Args, Page, undefined, Options, Context) ->
    search(Name, Args, Page, default_pagelen(Context), Options, Context);
search(Name, Args0, Page, PageLen, Options0, Context) when is_binary(Name), is_map(Options0) ->
    Args = props_to_map(Args0),
    Options = map_to_options(Options0),
    OffsetLimit = offset_limit(Page, PageLen, Options),
    Q = #search_query{
        name = Name,
        args = Args,
        offsetlimit = OffsetLimit,
        options = Options
    },
    case z_notifier:first(Q, Context) of
        undefined when Name =/= <<"query">> ->
            case m_rsc:rid(Name, Context) of
                RId when is_integer(RId) ->
                    case m_rsc:is_a(RId, 'query', Context) of
                        true ->
                            % Named query, perform the query with the stored arguments.
                            Args1 = Args#{
                                <<"query_id">> => RId
                            },
                            search(<<"query">>, Args1, Page, PageLen, Options, Context);
                        false ->
                            ?LOG_NOTICE(#{
                                text => <<"z_search: ignored unknown search query">>,
                                in => zotonic_core,
                                name => Name,
                                args => Args
                            }),
                            #search_result{
                                search_name = Name,
                                search_args = Args
                            }
                    end;
                undefined ->
                    ?LOG_NOTICE(#{
                        text => <<"z_search: ignored unknown search query">>,
                        in => zotonic_core,
                        name => Name,
                        args => Args
                    }),
                    #search_result{
                        search_name = Name,
                        search_args = Args
                    }
            end;
        undefined ->
            ?LOG_NOTICE(#{
                text => <<"z_search: ignored unknown search query">>,
                in => zotonic_core,
                name => Name,
                args => Args
            }),
            #search_result{
                search_name = Name,
                search_args = Args
            };
        Result ->
            handle_post_options(
                handle_search_result(Result, Page, PageLen, OffsetLimit, Name, Args, Options, Context),
                Context)
    end.


%% @doc Search items and handle the paging. Uses the default page length.
%% @deprecated use search/5
-spec search_pager(Query, Page, Context) -> Result when
    Query :: search_query(),
    Page :: pos_integer(),
    Context :: z:context(),
    Result :: #search_result{}.
search_pager(Search, Page, Context) ->
    search_pager(Search, Page, default_pagelen(Context), Context).

%% @doc Search items and handle the paging. This fetches extra rows beyond the requested
%% rows to ensure that the pager has the information for the "next page" options.
%% The number of extra rows depends on the current page, more for page 1, less for later pages.
-spec search_pager(Query, Page, PageLen, Context) -> Result when
    Query :: search_query(),
    Page :: pos_integer() | undefined,
    PageLen :: pos_integer() | undefined,
    Context :: z:context(),
    Result :: #search_result{}.
search_pager(Search, undefined, PageLen, Context) ->
    search_pager(Search, 1, PageLen, Context);
search_pager(Search, Page, undefined, Context) ->
    search_pager(Search, Page, default_pagelen(Context), Context);
search_pager({Name, Args}, Page, PageLen, Context) when is_binary(Name) ->
    search(Name, Args, Page, PageLen, Context);
search_pager({Name, Args}, _Page, PageLen, _Context) when PageLen < 1 ->
    #search_result{
        search_name = Name,
        search_args = Args,
        result = [],
        page = 1,
        pagelen = PageLen,
        pages = 0,
        prev = 1,
        next = false
    };
search_pager({Name, Args} = Search, Page, PageLen, Context) when is_atom(Name) ->
    OffsetLimit = offset_limit(Page, PageLen, #{}),
    SearchResult = search_1(Search, Page, PageLen, OffsetLimit, Context),
    handle_search_result(SearchResult, Page, PageLen, OffsetLimit, Name, Args, #{}, Context).


%% @doc Search with the question and return the results
%% @deprecated use search/5
-spec search({atom()|binary(), proplists:proplist()|map()}, z:context()) -> #search_result{}.
search(Search, Context) ->
    search(Search, default_offset_limit(Context), Context).

%% @doc Perform the named search and its arguments
%% @deprecated use search/5
-spec search(search_query(), search_offset() | undefined, z:context() ) -> #search_result{}.
search(Search, undefined, Context) ->
    search_1(Search, 1, ?SEARCH_ALL_LIMIT, {1, ?SEARCH_ALL_LIMIT}, Context);
search(Search, MaxRows, Context) when is_integer(MaxRows) ->
    search_1(Search, 1, MaxRows, {1, MaxRows}, Context);
search(Search, {1, Limit} = OffsetLimit, Context) ->
    search_1(Search, 1, Limit, OffsetLimit, Context);
search(Search, {Offset, Limit} = OffsetLimit, Context) ->
    case (Offset - 1) rem Limit of
        0 ->
            % On a page boundary, we can calculate the page number.
            PageNr = (Offset - 1) div Limit + 1,
            search_1(Search, PageNr, Limit, OffsetLimit, Context);
        _ ->
            % Not on a page boundary, give up on calculating the page number.
            search_1(Search, 1, ?SEARCH_ALL_LIMIT, OffsetLimit, Context)
    end.

-spec default_pagelen( z:context() ) -> pos_integer().
default_pagelen(Context) ->
    case m_config:get_value(site, pagelen, Context) of
        undefined -> ?SEARCH_PAGELEN;
        <<>> -> ?SEARCH_PAGELEN;
        Len -> z_convert:to_integer(Len)
    end.

-spec default_offset_limit( z:context() ) -> search_offset().
default_offset_limit(Context) ->
    {1, default_pagelen(Context)}.


%% @doc Find the term value of a named argument in a query terms list or map.
-spec lookup_qarg_value(Arg, Terms) -> TermValue | undefined when
    Arg :: binary(),
    Terms :: map(),
    TermValue :: term().
lookup_qarg_value(Arg, Terms) ->
    lookup_qarg_value(Arg, Terms, undefined).

%% @doc Find the term value of a named argument in a query terms list or map. Return
%% the default when the term is not found or doesn't have a value.
-spec lookup_qarg_value(Arg, Terms, Default) -> TermValue | Default when
    Arg :: binary(),
    Terms :: map(),
    Default :: term(),
    TermValue :: term().
lookup_qarg_value(Arg, Terms, Default) ->
    case lookup_qarg(Arg, Terms, undefined) of
        #{ <<"value">> := V } -> V;
        #{} -> Default;
        undefined -> Default
    end.

%% @doc Find a named argument in a query terms list or map.
-spec lookup_qarg(Arg, Terms) -> Term | undefined when
    Arg :: binary(),
    Terms :: map(),
    Term :: #{ binary() => term() } | undefined.
lookup_qarg(Arg, Terms) ->
    lookup_qarg(Arg, Terms, undefined).

-spec lookup_qarg(Arg, Terms, Default) -> Term | Default when
    Arg :: binary(),
    Terms :: map(),
    Default :: term(),
    Term :: #{ binary() => term() }.
lookup_qarg(Arg, #{ <<"q">> := Terms }, Default) when is_list(Terms) ->
    lookup_qarg_1(Arg, Terms, Default).

lookup_qarg_1(_Arg, [], Default) ->
    Default;
lookup_qarg_1(Arg, [ #{ <<"term">> := T } = Term | _ ], _Default) when T =:= Arg ->
    Term;
lookup_qarg_1(Arg, [ _ | Terms ], Default) ->
    lookup_qarg_1(Arg, Terms, Default).


%% @doc Handle a return value from a search function.  This can be an intermediate SQL statement
%% that still needs to be augmented with extra ACL checks.
-spec handle_search_result( Result, Page, PageLen, OffsetLimit, Name, Args, Options, Context ) -> #search_result{} when
    Result :: list() | #search_result{} | #search_sql{},
    Page :: pos_integer(),
    PageLen :: pos_integer(),
    OffsetLimit :: {non_neg_integer(), non_neg_integer()},
    Name :: binary() | atom(),
    Args :: map() | proplists:proplist(),
    Options :: search_options(),
    Context :: z:context().
handle_search_result(#search_result{ result = L, total = Total } = S, Page, PageLen, _OffsetLimit, Name, Args, Options, _Context)
    when is_integer(Total) ->
    Pages = (Total+PageLen-1) div PageLen,
    Next = if
        Page < Pages -> Page + 1;
        true -> false
    end,
    S#search_result{
        search_name = Name,
        search_args = Args,
        result = lists:sublist(L, 1, PageLen),
        options = Options,
        page = Page,
        pagelen = PageLen,
        pages = Pages,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(#search_result{ result = L, total = undefined } = S, Page, PageLen, _OffsetLimit, Name, Args, Options, _Context) ->
    % Search result without page nr, but which should have used the OffsetLimit.
    Len = length(L),
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    S#search_result{
        search_name = Name,
        search_args = Args,
        result = lists:sublist(L, 1, PageLen),
        options = Options,
        page = Page,
        pagelen = PageLen,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(L, Page, PageLen, {Offset, _Limit}, Name, Args, Options, _Context) when is_list(L) ->
    % Simple search results that don't do their paging and offset/limit handling, do the paging here.
    Total = length(L),
    Pages = (Total + PageLen - 1) div PageLen,
    Next = if
        Page < Pages -> Page + 1;
        true -> false
    end,
    #search_result{
        search_name = Name,
        search_args = Args,
        result = sublist(L, Offset, PageLen),
        page = Page,
        pagelen = PageLen,
        pages = Pages,
        options = Options,
        total = Total,
        is_total_estimated = false,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(#search_sql_terms{} = Terms, Page, PageLen, OffsetLimit, Name, Args, Options, Context) ->
    SearchSQL = z_search_terms:combine(Terms),
    handle_search_result(SearchSQL, Page, PageLen, OffsetLimit, Name, Args, Options, Context);
handle_search_result(#search_sql{} = Q, Page, PageLen, {_, Limit} = OffsetLimit, Name, Args, Options, Context) ->
    Q1 = reformat_sql_query(Q, Options, Context),
    {Sql, SqlArgs} = concat_sql_query(Q1, OffsetLimit),
    case Q#search_sql.run_func of
        F when is_function(F) ->
            Result = F(Q, Sql, Args, Context),
            handle_search_result(Result, Page, PageLen, OffsetLimit, Name, Args, Options, Context);
        _ ->
            Rows = case Q#search_sql.assoc of
                false ->
                    Rs = z_db:q(Sql, SqlArgs, Context),
                    case Rs of
                        [{_}|_] -> [ R || {R} <- Rs ];
                        _ -> Rs
                    end;
                true ->
                    z_db:assoc_props(Sql, SqlArgs, Context)
            end,
            RowCount = length(Rows),
            FoundTotal = (Page-1) * PageLen + RowCount,
            {Total, IsEstimate} = if
                Limit > RowCount andalso RowCount > 0 ->
                    % Didn't return all rows, assume we are at the end of the result set.
                    {FoundTotal, false};
                true ->
                    % No rows or the number of requested rows was returned.
                    {SqlNoLimit, ArgsNoLimit} = concat_sql_query(Q1, undefined),
                    {ok, EstimatedTotal} = z_db:estimate_rows(SqlNoLimit, ArgsNoLimit, Context),
                    if
                        RowCount > 0 ->
                            % Max rows returned, the total is more
                            {erlang:max(FoundTotal, EstimatedTotal), true};
                        RowCount =:= 0 andalso Page =:= 1 ->
                            {0, false};
                        RowCount =:= 0 ->
                            % We are beyond the number of available rows
                            {erlang:min(FoundTotal, EstimatedTotal), true}
                    end
            end,
            Pages = (Total + PageLen - 1) div PageLen,
            Next = if
                Pages > Page -> Page + 1;
                true -> false
            end,
            Result = #search_result{
                search_name = Name,
                search_args = Args,
                result = lists:sublist(Rows, 1, PageLen),
                options = Options,
                pages = Pages,
                page = Page,
                pagelen = PageLen,
                total = Total,
                is_total_estimated = IsEstimate,
                prev = erlang:max(Page-1, 1),
                next = Next
            },
            case Q#search_sql.post_func of
                undefined -> Result;
                Fun -> Fun(Result, Q1, Context)
            end
    end.

%% Calculate an offset/limit for the query. This takes such a range from the results
%% that we can display a pager. We don't need exact results, as we will use the query
%% planner to give an estimated number of rows.
offset_limit(_Page, _PageLen, #{ is_count_rows := true }) ->
    {1, 1};
offset_limit(1, PageLen, _Options) ->
    % Take 6 pages + 1 or the MIN_LOOKAHEAD
    {1, erlang:max(6 * PageLen + 1, ?MIN_LOOKAHEAD)};
offset_limit(N, PageLen, _Options) ->
    % Take at least 5 pages + 1 to accomodate for the default slider of the pager.
    {(N-1) * PageLen + 1, erlang:max(5 * PageLen + 1, ?MIN_LOOKAHEAD - N * PageLen)}.


sublist(L, Offset, Limit) ->
    try
        lists:sublist(L, Offset, Limit)
    catch
        error:function_clause ->
            []
    end.

%% @doc Handle deprecated searches in the {atom, list()} format.
search_1({SearchName, Props}, Page, PageLen, _OffsetLimit, Context) when is_binary(SearchName), is_integer(Page) ->
    ArgsMap = props_to_map(Props),
    search(SearchName, ArgsMap, Page, PageLen, Context);
search_1({SearchName, Props}, Page, PageLen, {Offset, Limit} = OffsetLimit, Context) when is_atom(SearchName), is_list(Props) ->
    Props1 = case proplists:get_all_values(cat, Props) of
        [] -> Props;
        [[]] -> Props;
        Cats -> [{cat, Cats} | proplists:delete(cat, Props)]
    end,
    Props2 = case proplists:get_all_values(cat_exclude, Props1) of
        [] -> Props1;
        [[]] -> Props1;
        CatsX -> [{cat_exclude, CatsX} | proplists:delete(cat_exclude, Props1)]
    end,
    PropsSorted = lists:keysort(1, Props2),
    Q = #search_query{
        search={SearchName, PropsSorted},
        offsetlimit=OffsetLimit
    },
    PageRest = (Offset - 1) rem PageLen,
    case z_notifier:first(Q, Context) of
        undefined when PageRest =:= 0 ->
            % Nicely on a page boundary, try the new search with binary search names.
            PageNr = (Offset - 1) div PageLen + 1,
            SearchNameBin = z_convert:to_binary(SearchName),
            ArgsMap = props_to_map(PropsSorted),
            search(SearchNameBin, ArgsMap, PageNr, PageLen, Context);
        undefined ->
            % Not on a page boundary so we can't use the new search, return the empty result.
            ?LOG_NOTICE(#{
                text => <<"z_search: ignored unknown search query">>,
                in => zotonic_core,
                name => SearchName,
                args => PropsSorted
            }),
            #search_result{};
        Result when Page =/= undefined ->
            handle_search_result(Result, Page, PageLen, OffsetLimit, SearchName, PropsSorted, #{}, Context);
        Result when PageRest =:= 0 ->
            PageNr = (Offset - 1) div Limit + 1,
            handle_search_result(Result, PageNr, Limit, OffsetLimit, SearchName, PropsSorted, #{}, Context);
        Result ->
            S = search_result(Result, OffsetLimit, Context),
            S#search_result{
                search_name = SearchName,
                search_args = PropsSorted
            }
    end;
search_1(Name, Page, PageLen, OffsetLimit, Context) when is_atom(Name) ->
    search_1({Name, []}, Page, PageLen, OffsetLimit, Context);
search_1(Name, _Page, _PageLen, _OffsetLimit, _Context) ->
    ?LOG_NOTICE(#{
        text => <<"z_search: ignored unknown search query">>,
        in => zotonic_core,
        name => Name
    }),
    #search_result{}.


%% @doc Handle the options applied after a search has been done.
-spec handle_post_options(#search_result{}, z:context()) -> #search_result{}.
handle_post_options(#search_result{ options = Options } = S, Context) ->
    maybe_option_properties(Options, S, Context).

maybe_option_properties(#{ properties := Props }, #search_result{ result = Result } = S, Context) ->
    Result1 = lists:filtermap(
        fun(R) ->
            option_properties(R, Props, Context)
        end,
        Result),
    S#search_result{ result = Result1 };
maybe_option_properties(_Options, S, _Context) ->
    S.

option_properties(R, true, Context) ->
    DefaultProps = [
        <<"id">>,
        <<"title">>,
        <<"short_title">>,
        <<"summary">>,
        <<"category_id">>,
        <<"category">>,
        <<"page_url">>,
        <<"thumbnail_url">>
    ],
    option_properties(R, DefaultProps, Context);
option_properties(Id, Props, Context) when is_integer(Id), is_list(Props) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            Rsc = lists:foldl(
                fun
                    (<<"category">>, Acc) ->
                        CatId = m_rsc:p(Id, <<"category_id">>, Context),
                        Acc#{
                            <<"category">> => #{
                                <<"id">> => CatId,
                                <<"name">> => m_rsc:p(CatId, <<"name">>, Context),
                                <<"title">> => m_rsc:p(CatId, <<"title">>, Context)
                            }
                        };
                    (<<"o.", Predicate/binary>> = P, Acc) ->
                        OProps = sub_properties(m_edge:objects(Id, Predicate, Context), Context),
                        Acc#{
                            P => OProps
                        };
                    (<<"s.", Predicate/binary>> = P, Acc) ->
                        SProps = sub_properties(m_edge:subjects(Id, Predicate, Context), Context),
                        Acc#{
                            P => SProps
                        };
                    (P, Acc) when is_binary(P) ->
                        Acc#{
                            P => m_rsc:p(Id, P, Context)
                        };
                    (_, Acc) ->
                        Acc
                end,
                #{ <<"id">> => Id },
                Props),
            {true, Rsc};
        false ->
            false
    end;
option_properties({Title, Id}, Props, Context) when is_integer(Id) ->
    % Typical result from a search bytitle (or another sort key)
    case option_properties(Id, Props, Context) of
        {true, Rsc} ->
            {true, {Title, Rsc}};
        false ->
            false
    end;
option_properties(R, _Props, _Context) ->
    R.


sub_properties(Ids, Context) ->
    lists:filtermap(
        fun(Id) ->
            case z_acl:rsc_visible(Id, Context) of
                true ->
                    {true, option_properties(Id, true, Context)};
                false ->
                    false
            end
        end,
        Ids).


%% @doc Map a map with (binary) search options to a search option list.
-spec map_to_options( map() ) -> search_options().
map_to_options(Map) ->
    maps:fold(
        fun
            (properties, V, Acc) ->
                Acc#{ properties => opt_props(V) };
            (<<"properties">>, V, Acc) ->
                Acc#{ properties => opt_props(V) };
            (is_count_rows, V, Acc) ->
                Acc#{ is_count_rows => z_convert:to_bool(V) };
            (<<"is_count_rows">>, V, Acc) ->
                Acc#{ is_count_rows => z_convert:to_bool(V) };
            (K, V, Acc) ->
                ?LOG_INFO(#{
                    text => <<"Dropping unknown search option">>,
                    in => zotonic_core,
                    option => K,
                    value => V
                }),
                Acc
        end,
        #{},
        Map).

opt_props(<<>>) ->
    true;
opt_props(B) when is_binary(B) ->
    opt_props([B]);
opt_props([]) ->
    true;
opt_props(L) when is_list(L) ->
    lists:flatten([
        binary:split(z_convert:to_binary(P), <<",">>, [global, trim]) || P <- L
    ]);
opt_props(P) ->
    z_convert:to_bool(P).

%% @doc Change a search props list to a standard query format.
-spec props_to_map( list() | map() | undefined ) -> #{ binary() => term() }.
props_to_map(Props) when is_map(Props) ->
    z_search_props:from_map(Props);
props_to_map(Props) when is_list(Props) ->
    z_search_props:from_list(Props);
props_to_map(undefined) ->
    z_search_props:from_list([]).


%% @doc Given a query as proplist or map, return all results.
-spec query_( proplists:proplist() | map(), z:context() ) -> list( m_rsc:resource_id() ).
query_(Args, Context) when is_map(Args) ->
    S = search(<<"query">>, Args, 1, ?SEARCH_ALL_LIMIT, Context),
    S#search_result.result;
query_(Props, Context) ->
    % deprecated, should use argument maps.
    S = search({'query', Props}, ?SEARCH_ALL_LIMIT, Context),
    S#search_result.result.


%% @doc Handle a return value from a search function.  This can be an intermediate SQL statement that still needs to be
%% augmented with extra ACL checks.
-spec search_result(Result, search_offset(), Context) -> #search_result{} when
    Result :: list() | #search_result{} | #search_sql{},
    Context :: z:context().
search_result(L, _Limit, _Context) when is_list(L) ->
    #search_result{
        result = L
    };
search_result(#search_result{} = S, _Limit, _Context) ->
    S;
search_result(#search_sql{} = Q, Limit, Context) ->
    Q1 = reformat_sql_query(Q, #{}, Context),
    {Sql, Args} = concat_sql_query(Q1, Limit),
    case Q#search_sql.run_func of
        F when is_function(F) ->
            F(Q, Sql, Args, Context);
        _ ->
            Rows = case Q#search_sql.assoc of
                false ->
                    Rs = z_db:q(Sql, Args, Context),
                    case Rs of
                        [{_}|_] ->
                            [ R || {R} <- Rs ];
                        _ ->
                            Rs
                    end;
                true ->
                    z_db:assoc_props(Sql, Args, Context)
            end,
            #search_result{
                result = Rows
            }
    end.


concat_sql_query(#search_sql{
        select = Select,
        from = From,
        where = Where,
        group_by = GroupBy,
        order = Order,
        limit = SearchLimit,
        args = Args
    }, Limit1) ->
    From1  = concat_sql_from(From),
    Where1 = case Where of
        "" -> "";
        <<>> -> <<>>;
        _ -> [ "where ", Where ]
    end,
    Order1 = case Order of
        "" -> "";
        <<>> -> <<>>;
        _ -> [ "order by ", Order ]
    end,
    GroupBy1 = case GroupBy of
        "" -> "";
        <<>> -> <<>>;
        _ -> [ "group by ", GroupBy ]
    end,
    {Parts, FinalArgs} = case SearchLimit of
        undefined ->
            case Limit1 of
                undefined ->
                    %% No limit. Use with care.
                    {[
                        "select", Select,
                        "from", From1,
                        Where1,
                        GroupBy1,
                        Order1
                    ], Args};
                {OffsetN, LimitN} ->
                    N = length(Args),
                    Args1 = Args ++ [OffsetN-1, LimitN],
                    {[
                        "select", Select,
                        "from", From1,
                        Where1,
                        GroupBy1,
                        Order1,
                        [ "offset $", integer_to_list(N+1) ],
                        [ "limit $", integer_to_list(N+2) ]
                    ], Args1}
            end;
        _ ->
            {[
                "select", Select,
                "from", From1,
                Where1,
                GroupBy1,
                Order1,
                SearchLimit
            ], Args}
    end,
    {iolist_to_binary( lists:join(" ", Parts) ), FinalArgs}.

%% @doc Inject the ACL checks in the SQL query.
-spec reformat_sql_query(#search_sql{}, search_options(), z:context()) -> #search_sql{}.
reformat_sql_query(#search_sql{where=Where, from=From, tables=Tables, args=Args,
                               cats=TabCats, cats_exclude=TabCatsExclude,
                               cats_exact=TabCatsExact} = Q,
                    Options,
                    Context) ->
    {ExtraWhere, Args1} = lists:foldl(
                                fun(Table, {Acc,As}) ->
                                    {W,As1} = add_acl_check(Table, As, Q, Context),
                                    {[W|Acc], As1}
                                end, {[], Args}, Tables),
    {From1, ExtraWhere1} = lists:foldl(
                             fun({Alias, Cats}, {F, C}) ->
                                     case add_cat_check(F, Alias, false, Cats, Context) of
                                         {_, []} -> {F, C};
                                         {FromNew, CatCheck} -> {FromNew, [CatCheck | C]}
                                     end
                             end, {From, ExtraWhere}, TabCats),
    {From2, ExtraWhere2} = lists:foldl(
                                fun({Alias, Cats}, {F, C}) ->
                                    case add_cat_check(F, Alias, true, Cats, Context) of
                                        {_, []} -> {F, C};
                                        {FromNew, CatCheck} -> {FromNew, [CatCheck | C]}
                                    end
                                end, {From1, ExtraWhere1}, TabCatsExclude),
    {ExtraWhere3, Args2} = lists:foldl(
                                fun({Alias, Cats}, {WAcc,As}) ->
                                    add_cat_exact_check(Cats, Alias, WAcc, As, Context)
                                end, {ExtraWhere2, Args1}, TabCatsExact),
    Where1 = case Where of
        <<>> -> [];
        B when is_binary(B) -> [ B ];
        L when is_list(L) -> L
    end,
    Where2 = iolist_to_binary(concat_where(ExtraWhere3, Where1)),
    Q1 = Q#search_sql{ where=Where2, from=From2, args=Args2 },
    case Options of
        #{ is_count_rows := true } ->
            Q1#search_sql{
                select = "count(*)",
                limit = ""
            };
        #{} ->
            Q1
    end.

%% @doc Concatenate the where clause with the extra ACL checks using "and".  Skip empty clauses.
concat_where([], Acc) ->
    Acc;
concat_where([<<>>|Rest], Acc) ->
    concat_where(Rest, Acc);
concat_where([[]|Rest], Acc) ->
    concat_where(Rest, Acc);
concat_where([W|Rest], []) ->
    concat_where(Rest, [W]);
concat_where([W|Rest], Acc) ->
    concat_where(Rest, [W, " and " | Acc]).


%% @doc Process SQL from clause. Analyzing the input (it may be a string, list of #search_sql or/and other strings)
concat_sql_from(From) ->
    Froms = concat_sql_from1(From),
    lists:join(",", Froms).

concat_sql_from1(From) when is_binary(From) ->
    [ From ]; %% from is string
concat_sql_from1([ H | _ ] = From) when is_integer(H) ->
    [ From ]; %% from is string?
concat_sql_from1([ #search_sql{} = From | T ]) ->
    Subquery = case concat_sql_query(From, undefined) of
    	{SQL, []} ->
            %% postgresql: alias for inner SELECT in FROM must be defined
            iolist_to_binary([
                "(", SQL, ") AS z_", z_ids:id()
                ]);
	   {SQL, [ {as, Alias} ]} when is_list(Alias); is_binary(Alias) ->
            iolist_to_binary([
                "(", SQL, ") AS ", Alias
                ]);
    	{_SQL, A} ->
            throw({badarg, "Use outer #search_sql.args to store args of inner #search_sql. Inner arg.list only can be equals to [] or to [{as, Alias=string()}] for aliasing innered select in FROM (e.g. FROM (SELECT...) AS Alias).", A})
    end,
    [ Subquery | concat_sql_from1(T) ];
concat_sql_from1([ {Source,Alias} | T ]) ->
    Alias2 = case z_utils:is_empty(Alias) of
        false ->
            [ " AS ", z_convert:to_binary(Alias) ];
        _ ->
            []
    end,
    [ [ concat_sql_from1(Source), Alias2 ] | concat_sql_from1(T) ];
concat_sql_from1([H|T]) ->
    [ concat_sql_from1(H) | concat_sql_from1(T) ];
concat_sql_from1([]) ->
    [];
concat_sql_from1(Something) ->
    % make list for records or other stuff
    concat_sql_from1([ Something ]).



%% @doc Create extra 'where' conditions for checking the access control
add_acl_check({rsc, Alias}, Args, Q, Context) ->
    add_acl_check_rsc(Alias, Args, Q, Context);
add_acl_check({<<"rsc">>, Alias}, Args, Q, Context) ->
    add_acl_check_rsc(Alias, Args, Q, Context);
add_acl_check(_, Args, _Q, _Context) ->
    {[], Args}.


%% @doc Create extra 'where' conditions for checking the access control
%% @todo This needs to be changed for the pluggable ACL
add_acl_check_rsc(Alias, Args, SearchSql, Context) ->
    case z_notifier:first(#acl_add_sql_check{alias=Alias, args=Args, search_sql=SearchSql}, Context) of
        undefined ->
            case z_acl:is_admin(Context) of
                true ->
                    % Admin can see all resources
                    {[], Args};
                false ->
                    %% Others can only see published resources
                    {publish_check(Alias, SearchSql), Args}
            end;
        {_NewSql, _NewArgs} = Result ->
            Result
    end.


publish_check(Alias, #search_sql{extra=Extra}) ->
    case lists:member(no_publish_check, Extra) of
        true ->
            [];
        false ->
            [
                  Alias, ".is_published = true and "
                , Alias, ".publication_start <= now() and "
                , Alias, ".publication_end >= now()"
            ]
    end.


%% @doc Create the 'where' conditions for the category check
add_cat_check(_From, _Alias, _Exclude, [], _Context) ->
    {_From, []};
add_cat_check(From, Alias, Exclude, Cats, Context) ->
    case m_category:is_tree_dirty(Context) of
        false ->
            % Use range queries on the category_nr pivot column.
            add_cat_check_pivot(From, Alias, Exclude, Cats, Context);
        true ->
            % While the category tree is rebuilding, we use the less efficient version with joins.
            add_cat_check_joined(From, Alias, Exclude, Cats, Context)
    end.

add_cat_check_pivot(From, Alias, Exclude, Cats, Context) ->
    Ranges = m_category:ranges(Cats, Context),
    CatChecks = [ cat_check_pivot1(Alias, Exclude, Range) || Range <- Ranges ],
    case CatChecks of
        [] ->
            {From, []};
        [_CatCheck] ->
            {From, CatChecks};
        _ ->
            Sep = case Exclude of
                false -> " or ";
                true -> " and "
            end,
            {From, [ "(", lists:join(Sep, CatChecks), ")" ]}
    end.

cat_check_pivot1(Alias, false, {From,From}) ->
    [ Alias, ".pivot_category_nr = ", integer_to_list(From) ];
cat_check_pivot1(Alias, false, {From,To}) ->
    [ Alias, ".pivot_category_nr >= ", integer_to_list(From)
    , " and ", Alias, ".pivot_category_nr <= ", integer_to_list(To)
    ];

cat_check_pivot1(Alias, true, {From,From}) ->
    [ Alias, ".pivot_category_nr <> ", integer_to_list(From) ];
cat_check_pivot1(Alias, true, {From,To}) ->
    [ $(, Alias, ".pivot_category_nr < ", integer_to_list(From)
    , " or ", Alias, ".pivot_category_nr > ", integer_to_list(To), ")"
    ].


%% Add category tree range checks by using joins. Less optimal; only
%% used while the category tree is being recalculated.
add_cat_check_joined(From, Alias, Exclude, Cats, Context) ->
    Ranges = m_category:ranges(Cats, Context),
    CatAlias = [ Alias, "_cat" ],
    FromNew = [{"hierarchy", CatAlias}|From],
    CatChecks = lists:map(
        fun(Range) ->
            Check = cat_check_joined1(CatAlias, Exclude, Range),
            [
              Alias, ".category_id = ", CatAlias, ".id and "
            , CatAlias, ".name = '$category' and "
            , Check
            ]
        end,
        Ranges),
    case CatChecks of
        [] ->
            {From, []};
        [_CatCheck] ->
            {FromNew, CatChecks};
        _ ->
            Sep = case Exclude of
                false -> " or ";
                true -> " and "
            end,
            {FromNew, [
                "(",
                lists:join(Sep, CatChecks),
                ")"
            ]}
    end.

cat_check_joined1(CatAlias, false, {Left,Left}) ->
    [ CatAlias, ".nr = ", integer_to_list(Left) ];
cat_check_joined1(CatAlias, false, {Left,Right}) ->
    [ CatAlias, ".nr >= ", integer_to_list(Left)
    , " and ", CatAlias,  ".nr <= ", integer_to_list(Right)
    ];
cat_check_joined1(CatAlias, true, {Left,Left}) ->
    [ CatAlias, ".nr <> ", integer_to_list(Left) ];
cat_check_joined1(CatAlias, true, {Left,Right}) ->
    [ "(", CatAlias, ".nr < ", integer_to_list(Left)
    ," or ", CatAlias, ".nr > ", integer_to_list(Right), ")"
    ].

%% @doc Add a check for an exact category match
add_cat_exact_check([], _Alias, WAcc, As, _Context) ->
    {WAcc, As};
add_cat_exact_check(CatsExact, Alias, WAcc, As, Context) ->
    CatIds = [ m_rsc:rid(CId, Context) || CId <- CatsExact ],
    {WAcc ++ [
        [Alias, [ ".category_id = any($", (integer_to_list(length(As)+1)), "::int[])"] ]
     ],
     As ++ [CatIds]}.

