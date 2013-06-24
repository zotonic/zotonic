%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-15
%% @doc Search the database, interfaces to specific search routines.

%% Copyright 2009 Marc Worrell
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
    pager/3,
    pager/4
]).

-include_lib("zotonic.hrl").

-define(OFFSET_LIMIT, {1,?SEARCH_PAGELEN}).
-define(OFFSET_PAGING, {1,30000}).

%% @doc Search items and handle the paging.  Uses the default page length.
%% @spec search_pager({Name, SearchPropList}, Page, #context{}) -> #search_result{}
search_pager(Search, Page, Context) ->
    search_pager(Search, Page, ?SEARCH_PAGELEN, Context).

%% @doc Search items and handle the paging
%% @spec search_pager({Name, SearchPropList}, Page, PageLen, #context{}) -> #search_result{}
search_pager(Search, Page, PageLen, Context) ->
    SearchResult = search(Search, ?OFFSET_PAGING, Context),
    pager(SearchResult, Page, PageLen, Context).


pager(#search_result{pagelen=undefined} = SearchResult, Page, Context) ->
    pager(SearchResult, Page, ?SEARCH_PAGELEN, Context);
pager(SearchResult, Page, Context) ->
    pager(SearchResult, Page, SearchResult#search_result.pagelen, Context).

pager(#search_result{result=Result} = SearchResult, Page, PageLen, _Context) ->
    Total = length(Result),
    Pages = mochinum:int_ceil(Total / PageLen), 
    Offset = (Page-1) * PageLen + 1,
    OnPage = case Offset =< Total of
        true ->
            {P,_} = z_utils:split(PageLen, lists:nthtail(Offset-1, Result)),
            P;
        false ->
            []
    end,
    Next = if Offset + PageLen < Total -> false; true -> Page+1 end,
    Prev = if Page > 1 -> Page-1; true -> 1 end,
    SearchResult#search_result{
        result=OnPage,
        all=Result,
        total=Total,
        page=Page,
        pagelen=PageLen,
        pages=Pages,
        next=Next,
        prev=Prev
    }.

%% @doc Search with the question and return the results
%% @spec search({Name, SearchPropList}, #context{}) -> #search_result{}
search(Search, Context) ->
    search(Search, ?OFFSET_LIMIT, Context).

%% @doc Perform the named search and its arguments
%% @spec search({Name, SearchPropList}, {Offset, Limit}, #context{}) -> #search_result{}
search({SearchName, Props} = Search, OffsetLimit, Context) ->
    Props1 = case proplists:get_all_values(cat, Props) of
        [] -> Props;
        Cats -> [{cat, Cats} | proplists:delete(cat, Props)]
    end,
    Props2 = case proplists:get_all_values(cat_exclude, Props1) of
        [] -> Props1;
        CatsX -> [{cat_exclude, CatsX} | proplists:delete(cat_exclude, Props1)]
    end,
    PropsSorted = lists:keysort(1, Props2),
    Q = #search_query{search={SearchName, PropsSorted}, offsetlimit=OffsetLimit},
    case z_notifier:first(Q, Context) of
        undefined -> 
            ?LOG("Unknown search query ~p~n~p~n", [Search, erlang:get_stacktrace()]),
            #search_result{};
        Result when Result /= undefined -> 
            search_result(Result, OffsetLimit, Context)
    end;
search(Name, OffsetLimit, Context) ->
    search({z_convert:to_atom(Name), []}, OffsetLimit, Context).

%% @doc Given a query as proplist, return all results.
%% @spec query_(Props, Context) -> [Id] | []
query_(Props, Context) ->
    S = search({'query', Props}, ?OFFSET_PAGING, Context),
    S#search_result.result.

%% @doc Handle a return value from a search function.  This can be an intermediate SQL statement that still needs to be
%% augmented with extra ACL checks.
%% @spec search_result(Result, Limit, Context) -> #search_result{}
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


concat_sql_query(#search_sql{select=Select, from=From, where=Where, group_by=GroupBy, order=Order, limit=SearchLimit, args=Args}, Limit1) ->
    From1  = concat_sql_from(From),
    Where1 = case Where of
        [] -> [];
        _ -> "where " ++ Where
    end,
    Order1 = case Order of
        [] -> [];
        _ -> "order by "++Order
    end,
    GroupBy1 = case GroupBy of
        [] -> [];
        _ -> "group by "++GroupBy
    end,
    {Parts, FinalArgs} = case SearchLimit of
                             undefined ->
                                 case Limit1 of
                                     undefined ->
                                         %% No limit. Use with care.
                                         {["select", Select, "from", From1, Where1, GroupBy1, Order1], Args};
                                     {OffsetN, LimitN} ->
                                         N = length(Args),
                                         Args1 = Args ++ [OffsetN-1, LimitN],
                                         {["select", Select, "from", From1, Where1, GroupBy1, Order1, "offset", [$$|integer_to_list(N+1)], "limit", [$$|integer_to_list(N+2)]], Args1}
                                 end;
                             _ ->
                                 {["select", Select, "from", From1, Where1, GroupBy1, Order1, SearchLimit], Args}
                         end,
    {string:join(Parts, " "), FinalArgs}.
    

%% @doc Inject the ACL checks in the SQL query.
%% @spec reformat_sql_query(#search_sql{}, Context) -> #search_sql{}
reformat_sql_query(#search_sql{where=Where, from=From, tables=Tables, args=Args, cats=TabCats, cats_exclude=TabCatsExclude} = Q, Context) ->
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

    Where1 = lists:flatten(concat_where(ExtraWhere2, Where)),
    Q#search_sql{where=Where1, from=From2, args=Args1}.


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
%% @spec concat_sql_from(From) -> From1::string()
concat_sql_from(From) -> 
    Froms = concat_sql_from1(From),
    string:join(Froms, ",").

concat_sql_from1([H|_]=From) when is_integer(H) -> [From]; %% from is string?
concat_sql_from1([#search_sql{} = From | T]) ->
    Subquery = case concat_sql_query(From, undefined) of
	{SQL, []} -> "(" ++ SQL ++ ") AS z_"++z_ids:id(); %% postgresql: alias for inner SELECT in FROM must be defined
	{SQL, [{as, Alias}]} when is_list(Alias) -> "(" ++ SQL ++ ") AS " ++ Alias;
	{_SQL, A} -> throw({badarg, "Use outer #search_sql.args to store args of inner #search_sql. Inner arg.list only can be equals to [] or to [{as, Alias=string()}] for aliasing innered select in FROM (e.g. FROM (SELECT...) AS Alias).", A})
    end,
    [Subquery | concat_sql_from1(T)];
concat_sql_from1([{Source,Alias} | T]) ->
    Alias2 = case z_utils:is_empty(Alias) of
	false -> " AS " ++ z_convert:to_list(Alias);
	_     -> []
    end,
    [concat_sql_from1(Source) ++ Alias2 | concat_sql_from1(T) ];
concat_sql_from1([H|T]) ->
    [concat_sql_from1(H) | concat_sql_from1(T)];
concat_sql_from1([]) ->
    [];
concat_sql_from1(Something) ->	%% make list for records or other stuff
    concat_sql_from1([Something]).



%% @doc Create extra 'where' conditions for checking the access control
%% @spec add_acl_check({Table, Alias}, Args, Q, Context) -> {Where, NewArgs}
add_acl_check({rsc, Alias}, Args, Q, Context) ->
    add_acl_check1(rsc, Alias, Args, Q, Context);
add_acl_check(_, Args, _Q, _Context) ->
    {[], Args}.


%% @doc Create extra 'where' conditions for checking the access control
%% @spec add_acl_check1(Table, Alias, Args, SearchSQL, Context) -> {Where, NewArgs}
%% @todo This needs to be changed for the pluggable ACL
add_acl_check1(Table, Alias, Args, SearchSql, Context) ->
    case z_notifier:first(#acl_add_sql_check{alias=Alias, args=Args, search_sql=SearchSql}, Context) of
        undefined ->
            % N = length(Args),
            case z_acl:can_see(Context) of
                ?ACL_VIS_USER ->
                    % Admin or supervisor, can see everything
                    {[], Args};
                ?ACL_VIS_PUBLIC -> 
                    % Anonymous users can only see public published content
                    Sql = Alias ++ ".visible_for = 0" ++ publish_check(Table, Alias, SearchSql),
                    {Sql, Args};
                ?ACL_VIS_COMMUNITY -> 
                    % Only see published public or community content
                    Sql = Alias ++ ".visible_for in (0,1) " ++ publish_check(Table, Alias, SearchSql),
                    {Sql, Args};
                ?ACL_VIS_GROUP ->
                    % Can see published community and public content or any content from one of the user's groups
                    Sql = Alias ++ ".visible_for in (0,1) " ++ publish_check(Table, Alias, SearchSql),
                    N = length(Args),
                    Sql1 = "((" ++ Sql ++ ") or "++Alias++".id = $"++integer_to_list(N+1),
                    {Sql1, Args ++ [z_acl:user(Context)]}
            end;
        {_NewSql, _NewArgs} = Result ->
            Result
    end.


publish_check(rsc, Alias, #search_sql{extra=Extra}) ->
    case lists:member(no_publish_check, Extra) of
        true ->
            "";
        false ->
            " and "
            ++Alias++".is_published and "
            ++Alias++".publication_start <= now() and "
            ++Alias++".publication_end >= now()"
    end;
publish_check(_Table, _Alias, _SearchSql) ->
    "".


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
    FromNew = [{"category", CatAlias}|From],
    CatChecks = lists:map(fun(Range) ->
                                  Check = cat_check_joined1(CatAlias, Exclude, Range),
                                  Alias ++ ".category_id = " ++ CatAlias ++ ".id and " ++ Check
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
