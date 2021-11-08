%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2021 Marc Worrell
%% @doc Search the database, interfaces to specific search routines.

%% Copyright 2009-2021 Marc Worrell
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

    search/2,
    search/3,
    search_pager/3,
    search_pager/4,
    search_result/3,
    query_/2,

    props_to_map/1,

    estimate/3
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


%% @doc Perform a named search with arguments.
-spec search(Name, Args, Page, PageLen, Context) -> Result when
    Name :: binary(),
    Args :: map() | proplists:proplist() | undefined,
    Page :: pos_integer(),
    PageLen :: pos_integer(),
    Context :: z:context(),
    Result :: #search_result{}.
search(Name, undefined, Page, PageLen, Context) ->
    search(Name, #{}, Page, PageLen, Context);
search(Name, Args, Page, PageLen, Context) when is_list(Args) ->
    Args1 = props_to_map(Args),
    search(Name, Args1, Page, PageLen, Context);
search(Name, Args, Page, PageLen, Context) when is_binary(Name), is_map(Args) ->
    OffsetLimit = offset_limit(Page, PageLen),
    Q = #search_query{
        name = Name,
        args = Args,
        offsetlimit = OffsetLimit
    },
    case z_notifier:first(Q, Context) of
        undefined ->
            case m_rsc:rid(Name, Context) of
                RId when is_integer(RId), Name =/= <<"query">> ->
                    case m_rsc:is_a(RId, 'query', Context) of
                        true ->
                            % Named query, perform the query with the stored arguments.
                            Args1 = Args#{
                                <<"query_id">> => RId
                            },
                            search(<<"query">>, Args1, Page, PageLen, Context);
                        false ->
                            lager:debug("z_search: ignored unknown search query ~p with ~p", [ Name, Args ]),
                            #search_result{}
                    end;
                undefined ->
                    lager:debug("z_search: ignored unknown search query ~p with ~p", [ Name, Args ]),
                    #search_result{}
            end;
        Result ->
            handle_search_result(Result, Page, PageLen, OffsetLimit, Context)
    end.


%% @doc Search items and handle the paging. Uses the default page length.
%% @deprecated use search/5
-spec search_pager(search_query(), Page :: pos_integer(), z:context()) -> #search_result{}.
search_pager(Search, Page, Context) ->
    search_pager(Search, Page, ?SEARCH_PAGELEN, Context).

%% @doc Search items and handle the paging
-spec search_pager(search_query(), Page :: pos_integer(), PageLen :: pos_integer(), z:context()) -> #search_result{}.
search_pager(Search, undefined, PageLen, Context) ->
    search_pager(Search, 1, PageLen, Context);
search_pager({Name, Args}, Page, PageLen, Context) when is_binary(Name) ->
    search(Name, Args, Page, PageLen, Context);
search_pager({Name, _} = Search, Page, PageLen, Context) when is_atom(Name) ->
    OffsetLimit = offset_limit(Page, PageLen),
    SearchResult = search_1(Search, Page, PageLen, OffsetLimit, Context),
    handle_search_result(SearchResult, Page, PageLen, OffsetLimit, Context).


%% @doc Search with the question and return the results
%% @deprecated use search/5
-spec search({atom()|binary(), proplists:proplist()|map()}, z:context()) -> #search_result{}.
search(Search, Context) ->
    search(Search, ?OFFSET_LIMIT, Context).

%% @doc Perform the named search and its arguments
%% @deprecated use search/5
-spec search(search_query(), search_offset(), z:context() ) -> #search_result{}.
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
-spec handle_search_result( Result, Page, PageLen, OffsetLimit, Context ) -> #search_result{} when
    Result :: list() | #search_result{} | #search_sql{},
    Page :: pos_integer(),
    PageLen :: pos_integer(),
    OffsetLimit :: {non_neg_integer(), non_neg_integer()},
    Context :: z:context().
handle_search_result(#search_result{ result = L, total = Total } = S, Page, PageLen, _OffsetLimit, _Context) when is_integer(Total) ->
    L1 = lists:sublist(L, 1, PageLen),
    Pages = (Total+PageLen-1) div PageLen + Page - 1,
    Len = length(L),
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    S#search_result{
        result = L1,
        page = Page,
        pagelen = PageLen,
        pages = Pages,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(#search_result{ result = L, total = undefined } = S, Page, PageLen, _OffsetLimit,  _Context) ->
    L1 = lists:sublist(L, 1, PageLen),
    Len = length(L),
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    S#search_result{
        result = L1,
        page = Page,
        pagelen = PageLen,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(L, Page, PageLen, _OffsetLimit, _Context) when is_list(L) ->
    L1 = lists:sublist(L, 1, PageLen),
    Len = length(L),
    Pages = (Len+PageLen-1) div PageLen + Page - 1,
    Next = if
        Len > PageLen -> Page + 1;
        true -> false
    end,
    #search_result{
        result = L1,
        page = Page,
        pagelen = PageLen,
        pages = Pages,
        prev = erlang:max(Page-1, 1),
        next = Next
    };
handle_search_result(#search_sql{} = Q, Page, PageLen, {_, Limit} = OffsetLimit, Context) ->
    Q1 = reformat_sql_query(Q, Context),
    {Sql, Args} = concat_sql_query(Q1, OffsetLimit),
    case Q#search_sql.run_func of
        F when is_function(F) ->
            Result = F(Q, Sql, Args, Context),
            handle_search_result(Result, Page, PageLen, OffsetLimit, Context);
        _ ->
            Rows = case Q#search_sql.assoc of
                false ->
                    Rs = z_db:q(Sql, Args, Context),
                    case Rs of
                        [{_}|_] -> [ R || {R} <- Rs ];
                        _ -> Rs
                    end;
                true ->
                    z_db:assoc_props(Sql, Args, Context)
            end,
            RowCount = length(Rows),
            FoundTotal = (Page-1) * PageLen + RowCount,
            Total = if
                Limit > RowCount ->
                    % Didn't return all rows, assume we are at the end of the result set.
                    FoundTotal;
                true ->
                    % The number of requested rows was returned, assume there is more.
                    {SqlNoLimit, ArgsNoLimit} = concat_sql_query(Q1, undefined),
                    {ok, EstimatedTotal} = estimate(SqlNoLimit, ArgsNoLimit, Context),
                    erlang:max(FoundTotal, EstimatedTotal)
            end,
            Pages = (Total + PageLen - 1) div PageLen,
            Next = if
                Pages > Page -> Page + 1;
                true -> false
            end,
            #search_result{
                result = lists:sublist(Rows, 1, PageLen),
                pages = Pages,
                page = Page,
                pagelen = PageLen,
                total = Total,
                prev = erlang:max(Page-1, 1),
                next = Next
            }
    end.

%% @doc Estimate the number of rows matching a query. This uses the PostgreSQL query planner
%% to return an estimate of the number of rows.
-spec estimate(Query, Args, Context) -> {ok, Rows} | {error, term()}
    when Query :: string() | binary(),
         Args :: list(),
         Context :: z:context(),
         Rows :: non_neg_integer().
estimate(Query, Args, Context) ->
    Query1 = "explain " ++ z_convert:to_list(Query),
    try
        find_estimate( z_db:q(Query1, Args, Context) )
    catch
        throw:{error, _} = Error -> Error
    end.

find_estimate([]) ->
    {ok, 0};
find_estimate([{R}|Rs]) ->
    case re:run(R, <<" rows=([0-9]+)">>, [{capture, all_but_first, binary}]) of
        nomatch -> find_estimate(Rs);
        {match, [Rows]} -> {ok, binary_to_integer(Rows)}
    end.


%% Calculate an offset/limit for the query. This takes such a range from the results
%% that we can display a pager. We don't need exact results, as we will use the query
%% planner to give an estimated number of rows.
offset_limit(1, PageLen) ->
    % Take 5 pages + 1
    {1, 5 * PageLen + 1};
offset_limit(2, PageLen) ->
    % Take 4 pages + 1
    {PageLen + 1, 4 * PageLen + 1};
offset_limit(3, PageLen) ->
    % Take 3 pages + 1
    {2 * PageLen + 1, 3 * PageLen + 1};
offset_limit(N, PageLen) ->
    % Take 2 pages + 1
    {(N-1) * PageLen + 1, 2 * PageLen + 1}.


%% @doc Handle deprecated searches in the {atom, list()} format.
search_1({SearchName, Props}, undefined, _PageLen, {1, Limit}, Context) when is_binary(SearchName) ->
    ArgsMap = props_to_map(Props),
    search(SearchName, ArgsMap, 1, Limit, Context);
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
    Q = #search_query{search={SearchName, PropsSorted}, offsetlimit=OffsetLimit},
    PageRest = (Offset - 1) rem PageLen,
    case z_notifier:first(Q, Context) of
        undefined when PageRest =:= 0 ->
            % Nicely on a page boundary, try the new search with binary search names.
            PageNr = (Offset - 1) div PageLen + 1,
            SearchNameBin = z_convert:to_binary(SearchName),
            ArgsMap = props_to_map(PropsSorted),
            search(SearchNameBin, ArgsMap, PageNr, Limit, Context);
        undefined ->
            % Not on a page boundary so we can't use the new search, return the empty result.
            lager:info("z_search: ignored unknown search query ~p", [ {SearchName, PropsSorted} ]),
            #search_result{};
        Result when Page =/= undefined ->
            handle_search_result(Result, Page, PageLen, OffsetLimit, Context);
        Result when PageRest =:= 0 ->
            PageNr = (Offset - 1) div Limit + 1,
            handle_search_result(Result, PageNr, Limit, OffsetLimit, Context);
        Result ->
            search_result(Result, OffsetLimit, Context)
    end;
search_1(Name, Page, PageLen, OffsetLimit, Context) when is_atom(Name) ->
    search_1({Name, []}, Page, PageLen, OffsetLimit, Context);
search_1(Name, _Page, _PageLen, _OffsetLimit, _Context) ->
    lager:info("z_search: ignored unknown search query ~p", [ Name ]),
    #search_result{}.



%% @doc Change a search props list to a map with binary keys.
-spec props_to_map( proplists:proplist() | map() ) -> #{ binary() => term() }.
props_to_map(Props) when is_map(Props) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{ z_convert:to_binary(K) => V }
        end,
        #{},
        Props);
props_to_map(Props) ->
    {Map, _} = lists:foldr(
        fun({K, V}, {Acc, Counts}) ->
            K1 = z_convert:to_binary(K),
            case maps:get(K1, Counts, 0) of
                0 ->
                    Acc1 = Acc#{ K1 => V },
                    Counts1 = Counts#{ K1 => 1 },
                    {Acc1, Counts1};
                1 ->
                    V1 = #{
                        <<"all">> => [
                            V,
                            maps:get(K1, Acc)
                        ]
                    },
                    Acc1 = Acc#{ K1 => V1 },
                    Counts1 = Counts#{ K1 => 2 },
                    {Acc1, Counts1};
                N ->
                    #{ <<"all">> := Vs } = maps:get(K1, Acc),
                    V1 = #{
                        <<"all">> => [ V | Vs ]
                    },
                    Acc1 = Acc#{ K1 => V1 },
                    Counts1 = Counts#{ K1 => N + 1 },
                    {Acc1, Counts1}
            end
        end,
        {#{}, #{}},
        Props),
    Map.


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
-spec search_result( list() | #search_result{} | #search_sql{}, search_offset(), z:context() ) ->
    #search_result{}.
search_result(L, _Limit, _Context) when is_list(L) ->
    #search_result{result=L};
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
                        [{_}|_] -> [ R || {R} <- Rs ];
                        _ -> Rs
                    end;
                true ->
                    z_db:assoc_props(Sql, Args, Context)
            end,
            #search_result{result=Rows}
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
        [] -> [];
        _ -> [ "where ", Where ]
    end,
    Order1 = case Order of
        [] -> [];
        _ -> [ "order by ", Order ]
    end,
    GroupBy1 = case GroupBy of
        [] -> [];
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
                        "offset", [$$|integer_to_list(N+1)],
                        "limit", [$$|integer_to_list(N+2)]
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

    Where1 = lists:flatten(concat_where(ExtraWhere3, Where)),
    Q#search_sql{where=Where1, from=From2, args=Args2}.


%% @doc Concatenate the where clause with the extra ACL checks using "and".  Skip empty clauses.
%% @spec concat_where(ClauseList, CurrentWhere) -> NewClauseList
concat_where([], Acc) ->
    Acc;
concat_where([[]|Rest], Acc) ->
    concat_where(Rest, Acc);
concat_where([W|Rest], []) ->
    concat_where(Rest, [W]);
concat_where([W|Rest], Acc) ->
    concat_where(Rest, [W, " and "|Acc]).


%% @doc Process SQL from clause. We analyzing the input (it may be a string, list of #search_sql or/and other strings)
concat_sql_from(From) ->
    Froms = concat_sql_from1(From),
    lists:join(",", Froms).

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
	false -> " AS " ++ z_convert:to_list(Alias);
	_     -> []
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
            "";
        false ->
            " and "
            ++Alias++".is_published and "
            ++Alias++".publication_start <= now() and "
            ++Alias++".publication_end >= now()"
    end.


%% @doc Create the 'where' conditions for the category check
%% @spec add_cat_check(From, Alias, Exclude, Cats, Context) -> Where
add_cat_check(_From, _Alias, _Exclude, [], _Context) ->
    {_From, []};
add_cat_check(From, Alias, Exclude, Cats, Context) ->
    case m_category:is_tree_dirty(Context) of
        false ->
            add_cat_check_pivot(From, Alias, Exclude, Cats, Context);
        true ->
            %% While the category tree is rebuilding, we use the less efficient version with joins.
            add_cat_check_joined(From, Alias, Exclude, Cats, Context)
    end.

add_cat_check_pivot(From, Alias, Exclude, Cats, Context) ->
    Ranges = m_category:ranges(Cats, Context),
    CatChecks = [ cat_check_pivot1(Alias, Exclude, Range) || Range <- Ranges ],
    case CatChecks of
        [] -> {From, []};
        [_CatCheck] -> {From, CatChecks};
        _ -> {From, "(" ++ string:join(CatChecks, case Exclude of false -> " or "; true -> " and " end) ++ ")"}
    end.

cat_check_pivot1(Alias, false, {From,From}) ->
    Alias ++ ".pivot_category_nr = "++integer_to_list(From);
cat_check_pivot1(Alias, false, {From,To}) ->
    Alias ++ ".pivot_category_nr >= " ++ integer_to_list(From)
        ++ " and "++ Alias ++ ".pivot_category_nr <= " ++ integer_to_list(To);

cat_check_pivot1(Alias, true, {From,From}) ->
    Alias ++ ".pivot_category_nr <> "++integer_to_list(From);
cat_check_pivot1(Alias, true, {From,To}) ->
    [$(|Alias] ++ ".pivot_category_nr < " ++ integer_to_list(From)
        ++ " or "++ Alias ++ ".pivot_category_nr > " ++ integer_to_list(To) ++ ")".


%% Add category tree range checks by using joins. Less optimal; only
%% used while the category tree is being recalculated.
add_cat_check_joined(From, Alias, Exclude, Cats, Context) ->
    Ranges = m_category:ranges(Cats, Context),
    CatAlias = Alias ++ "_cat",
    FromNew = [{"hierarchy", CatAlias}|From],
    CatChecks = lists:map(fun(Range) ->
                                  Check = cat_check_joined1(CatAlias, Exclude, Range),
                                  Alias ++ ".category_id = " ++ CatAlias ++ ".id and "
                                  ++ CatAlias ++ ".name = '$category' and "
                                  ++ Check
                          end,
                          Ranges),
    case CatChecks of
        [] -> {From, []};
        [_CatCheck] -> {FromNew, CatChecks};
        _ -> {FromNew, "(" ++ string:join(CatChecks, case Exclude of false -> " or "; true -> " and " end) ++ ")"}
    end.

cat_check_joined1(CatAlias, false, {Left,Left}) ->
    CatAlias ++ ".nr = "++integer_to_list(Left);
cat_check_joined1(CatAlias, false, {Left,Right}) ->
      CatAlias ++ ".nr >= " ++ integer_to_list(Left)
        ++ " and "++ CatAlias ++ ".nr <= " ++ integer_to_list(Right);

cat_check_joined1(CatAlias, true, {Left,Left}) ->
    CatAlias ++ ".nr <> "++integer_to_list(Left);
cat_check_joined1(CatAlias, true, {Left,Right}) ->
    "(" ++ CatAlias ++ ".nr < " ++ integer_to_list(Left)
        ++ " or "++ CatAlias ++ ".nr > " ++ integer_to_list(Right) ++ ")".

%% @doc Add a check for an exact category match
add_cat_exact_check([], _Alias, WAcc, As, _Context) ->
    {WAcc, As};
add_cat_exact_check(CatsExact, Alias, WAcc, As, Context) ->
    CatIds = [ m_rsc:rid(CId, Context) || CId <- CatsExact ],
    {WAcc ++ [
        [Alias, ".category_id in (SELECT(unnest($"++(integer_to_list(length(As)+1))++"::int[])))"]
     ],
     As ++ [CatIds]}.

