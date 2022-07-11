%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell
%% @doc Search the database, interfaces to specific search routines.

%% Copyright 2009-2022 Marc Worrell
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
    search/2,
    search/3,
    search_pager/3,
    search_pager/4,
    search_result/3,
    query_/2,

    reformat_sql_query/2,
    concat_sql_query/2
]).

-include_lib("zotonic.hrl").

% The tuple format is deprecated. Use separate binary search term with a map.
-type search_query():: {atom(), list()}
                     | {binary(), map()|undefined}.
-type search_offset() :: Limit :: pos_integer()
                       | {Offset :: pos_integer(), Limit :: pos_integer()}.

-export_type([
    search_query/0,
    search_offset/0
    ]).

-define(OFFSET_LIMIT, {1,?SEARCH_PAGELEN}).
-define(SEARCH_ALL_LIMIT, 30000).
-define(MIN_LOOKAHEAD, 200).

%% @doc Search items and handle the paging. Uses the default page length.
%% @deprecated use search/5
-spec search_pager(search_query(), Page :: pos_integer(), z:context()) -> #search_result{}.
search_pager(Search, Page, Context) ->
    search_pager(Search, Page, ?SEARCH_PAGELEN, Context).

%% @doc Search items and handle the paging
-spec search_pager(search_query(), Page :: pos_integer(), PageLen :: pos_integer(), z:context()) -> #search_result{}.
search_pager(Search, undefined, PageLen, Context) ->
    search_pager(Search, 1, PageLen, Context);
search_pager({Name, Args} = Search, Page, PageLen, Context) when is_atom(Name) ->
    OffsetLimit = offset_limit(Page, PageLen),
    SearchResult = search_1(Search, Page, PageLen, OffsetLimit, Context),
    handle_search_result(SearchResult, Page, PageLen, OffsetLimit, Name, Args, Context).


%% @doc Search with the question and return the results
%% @deprecated use search/5
-spec search({atom()|binary(), proplists:proplist()}, z:context()) -> #search_result{}.
search(Search, Context) ->
    search(Search, ?OFFSET_LIMIT, Context).

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
            PageNr = (Offset - 1) div Limit,
            search_1(Search, PageNr, Limit, OffsetLimit, Context);
        _ ->
            search_1(Search, undefined, undefined, OffsetLimit, Context)
    end.


%% @doc Handle a return value from a search function.  This can be an intermediate SQL statement
%% that still needs to be augmented with extra ACL checks.
-spec handle_search_result( Result, Page, PageLen, OffsetLimit, Name, Args, Context ) -> #search_result{} when
    Result :: list() | #search_result{} | #search_sql{},
    Page :: pos_integer(),
    PageLen :: pos_integer(),
    OffsetLimit :: {non_neg_integer(), non_neg_integer()},
    Name :: binary() | atom(),
    Args :: proplists:proplist(),
    Context :: z:context().
handle_search_result(#search_result{ pages = N } = S, _Page, _PageLen, _OffsetLimit, Name, Args, _Context)
    when is_integer(N) ->
    S#search_result{
        search_name = Name,
        search_args = Args
    };
handle_search_result(#search_result{ result = L, total = Total } = S, Page, PageLen, _OffsetLimit, Name, Args, _Context)
    when is_integer(Total) ->
    L1 = lists:sublist(L, 1, PageLen),
    Pages = (Total+PageLen-1) div PageLen,
    Len = length(L),
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    S#search_result{
        search_name = Name,
        search_args = Args,
        result = L1,
        page = Page,
        pagelen = PageLen,
        pages = Pages,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(#search_result{ result = L, total = undefined } = S, Page, PageLen, _OffsetLimit, Name, Args, _Context) ->
    L1 = lists:sublist(L, 1, PageLen),
    Len = length(L),
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    S#search_result{
        search_name = Name,
        search_args = Args,
        result = L1,
        page = Page,
        pagelen = PageLen,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(L, Page, PageLen, _OffsetLimit, Name, Args, _Context) when is_list(L) ->
    L1 = lists:sublist(L, 1, PageLen),
    Len = length(L),
    Pages = (Len+PageLen-1) div PageLen + Page - 1,
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    #search_result{
        search_name = Name,
        search_args = Args,
        result = L1,
        page = Page,
        pagelen = PageLen,
        pages = Pages,
        total = Len,
        is_total_estimated = false,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(#search_sql{} = Q, Page, PageLen, {_, Limit} = OffsetLimit, Name, Args, Context) ->
    Q1 = reformat_sql_query(Q, Context),
    {Sql, SqlArgs} = concat_sql_query(Q1, OffsetLimit),
    case Q#search_sql.run_func of
        F when is_function(F) ->
            Result = F(Q, Sql, Args, Context),
            handle_search_result(Result, Page, PageLen, OffsetLimit, Name, Args, Context);
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
                Limit > RowCount ->
                    % Didn't return all rows, assume we are at the end of the result set.
                    {FoundTotal, false};
                true ->
                    % The number of requested rows was returned, assume there is more.
                    {SqlNoLimit, ArgsNoLimit} = concat_sql_query(Q1, undefined),
                    {ok, EstimatedTotal} = z_db:estimate_rows(SqlNoLimit, ArgsNoLimit, Context),
                    {erlang:max(FoundTotal, EstimatedTotal), true}
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
                pages = Pages,
                page = Page,
                pagelen = PageLen,
                total = Total,
                is_total_estimated = IsEstimate,
                prev = erlang:max(Page-1, 1),
                next = Next
            },
            Result
    end.

%% Calculate an offset/limit for the query. This takes such a range from the results
%% that we can display a pager. We don't need exact results, as we will use the query
%% planner to give an estimated number of rows.
offset_limit(1, PageLen) ->
    % Take 5 pages + 1
    {1, erlang:max(5 * PageLen + 1, ?MIN_LOOKAHEAD)};
offset_limit(2, PageLen) ->
    % Take 4 pages + 1
    {PageLen + 1, erlang:max(4 * PageLen + 1, ?MIN_LOOKAHEAD - PageLen)};
offset_limit(3, PageLen) ->
    % Take 3 pages + 1
    {2 * PageLen + 1, erlang:max(3 * PageLen + 1, ?MIN_LOOKAHEAD - 2 * PageLen)};
offset_limit(N, PageLen) ->
    % Take 2 pages + 1
    {(N-1) * PageLen + 1, erlang:max(2 * PageLen + 1, ?MIN_LOOKAHEAD - N * PageLen)}.


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
    Q = #search_query{search={SearchName, PropsSorted}, offsetlimit=OffsetLimit},
    PageRest = (Offset - 1) rem PageLen,
    case z_notifier:first(Q, Context) of
        undefined ->
            lager:info("[~p] Unknown search query ~p with ~p", [z_context:site(Context), SearchName, Props]),
            #search_result{};
        Result when Page =/= undefined ->
            handle_search_result(Result, Page, PageLen, OffsetLimit, SearchName, PropsSorted, Context);
        Result when PageRest =:= 0 ->
            PageNr = (Offset - 1) div Limit + 1,
            handle_search_result(Result, PageNr, Limit, OffsetLimit, SearchName, PropsSorted, Context);
        Result ->
            S = search_result(Result, OffsetLimit, Context),
            S#search_result{
                search_name = SearchName,
                search_args = PropsSorted
            }
    end;
search_1(Name, Page, PageLen, OffsetLimit, Context) when is_atom(Name) ->
    search_1({Name, []}, Page, PageLen, OffsetLimit, Context);
search_1(_Name, _Page, _PageLen, _OffsetLimit, _Context) ->
    #search_result{}.


%% @doc Given a query as proplist or map, return all results.
-spec query_( proplists:proplist(), z:context() ) -> list( m_rsc:resource_id() ).
query_(Props, Context) ->
    % deprecated, should use argument maps.
    S = search({'query', Props}, ?SEARCH_ALL_LIMIT, Context),
    S#search_result.result.


%% @doc Handle a return value from a search function.  This can be an intermediate SQL statement that still needs to be
%% augmented with extra ACL checks.
-spec search_result(Result , search_offset(), Context) ->
    #search_result{} when
    Result :: list() | #search_result{} | #search_sql{},
    Context :: z:context().
search_result(L, _Limit, _Context) when is_list(L) ->
    #search_result{
        result = L
    };
search_result(#search_result{} = S, _Limit, _Context) ->
    S;
search_result(#search_sql{} = Q, Limit, Context) ->
    Q1 = reformat_sql_query(Q, Context),
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
-spec reformat_sql_query(#search_sql{}, z:context()) -> #search_sql{}.
reformat_sql_query(#search_sql{where=Where, from=From, tables=Tables, args=Args,
                               cats=TabCats, cats_exclude=TabCatsExclude,
                               cats_exact=TabCatsExact} = Q, Context) ->
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
    Q#search_sql{where=Where2, from=From2, args=Args2}.


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
                " and "
                , Alias, ".is_published = true and "
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
        [Alias, [ ".category_id in (SELECT(unnest($", (integer_to_list(length(As)+1)), "::int[])))"] ]
     ],
     As ++ [CatIds]}.

