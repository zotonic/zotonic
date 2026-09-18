%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2025 Marc Worrell
%% @doc Combine search terms into a sql search query.
%% @end

%% Copyright 2021-2025 Marc Worrell
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

-module(z_search_terms).
-author("Marc Worrell <marc@worrell.nl").

-export([
    combine/1,
    combine/2,

    merge_args/2,
    map/2
    ]).


-include_lib("../../include/zotonic.hrl").

combine(Terms) ->
    combine(Terms, undefined).

combine(#search_sql_terms{ terms = Terms, post_func = PostFunc }, Context) ->
    Q = combine(Terms, Context),
    Q#search_sql{
        post_func = PostFunc
    };
combine(Terms, Context) when is_list(Terms) ->
    % Map all arguments before splitting nested terms into subqueries. This
    % keeps the argument numbers unique across the complete query.
    {Terms1, Args} = map_tree_args(Terms),
    AllAliases = defined_aliases(Terms1),
    {Terms2, Args1} = compile_terms(Terms1, AllAliases, #{}, Args, Context),
    Q0 = lists:foldr(fun merge_term/2, #search_sql_term{}, Terms2),
    Q1 = maybe_remove_default_select(Q0),
    Q = Q1#search_sql_term{ args = Args1 },
    Sort = optimize_sort(
        make_sort(Q#search_sql_term.asort, Q)
        ++ make_sort(Q#search_sql_term.sort, Q)
        ++ make_sort(Q#search_sql_term.zsort, Q)),
    From = iolist_to_binary([
        <<"rsc rsc">>,
        make_join(Q#search_sql_term.join_inner, "join"),
        make_join(Q#search_sql_term.join_left, "left join"),
        maps:fold(
            fun
                (<<"rsc">>, _, Acc) ->
                    Acc;
                (Alias, Table, Acc) ->
                    [ ", ", Table, " ", Alias, Acc ]
            end,
            <<>>,
            Q#search_sql_term.tables)
    ]),
    #search_sql{
        select = iolist_to_binary(lists:join(", ", Q#search_sql_term.select)),
        from = From,
        where = iolist_to_binary(lists:join(" AND ", Q#search_sql_term.where)),
        group_by = iolist_to_binary(lists:join(", ", Q#search_sql_term.group_by)),
        having = iolist_to_binary(lists:join(" AND ", Q#search_sql_term.having)),
        order = iolist_to_binary(lists:join(", ", Sort)),
        args = Q#search_sql_term.args,
        tables = resource_tables(
            Q#search_sql_term.tables,
            Q#search_sql_term.join_inner,
            Q#search_sql_term.join_left),
        cats = Q#search_sql_term.cats,
        cats_exact = Q#search_sql_term.cats_exact,
        cats_exclude = Q#search_sql_term.cats_exclude,
        extra = Q#search_sql_term.extra,
        search_sql_terms = Terms
    }.

%% @doc Remove full-text rank expressions which are assumed not to affect the
%% result after an explicit resource id, creation, or modification date sort.
%% Keep other expressions, as a SPARQL result can contain multiple rows for the
%% same resource.
optimize_sort(Sort) ->
    optimize_sort(Sort, false, []).

optimize_sort([], _IsComplete, Acc) ->
    lists:reverse(Acc);
optimize_sort([Sort | Rest], true, Acc) ->
    case is_fulltext_rank_sort(Sort) of
        true -> optimize_sort(Rest, true, Acc);
        false -> optimize_sort(Rest, true, [Sort | Acc])
    end;
optimize_sort([Sort | Rest], false, Acc) ->
    optimize_sort(Rest, is_complete_sort(Sort), [Sort | Acc]).

is_complete_sort(Sort) ->
    case re:run(
        iolist_to_binary(Sort),
        <<"^\\s*rsc\\.(?:id|created|modified)(?:\\s+(?:asc|desc))?"
          "(?:\\s+nulls\\s+(?:first|last))?\\s*$">>,
        [caseless])
    of
        {match, _} -> true;
        nomatch -> false
    end.

is_fulltext_rank_sort(Sort) ->
    case re:run(
        iolist_to_binary(Sort),
        <<"(?:ts_rank(?:_cd)?|(?:public\\.)?word_similarity)\\s*\\(">>,
        [caseless])
    of
        {match, _} -> true;
        nomatch -> false
    end.

%% In group_by clauses we do not need the rsc.id select, which is always added
%% by default (in the record definition).
maybe_remove_default_select(#search_sql_term{ extra = Extra, select = Select } = Q) ->
    case lists:member(no_default_select, Extra) of
        true -> Q#search_sql_term{ select = lists:delete(<<"rsc.id">>, Select) };
        false -> Q
    end.

make_join(Joins, JoinType) ->
    maps:fold(
        fun
            (Alias, {Table, OnClause}, Acc) ->
                [
                    Acc, " ", JoinType,
                    " ", Table, " ", Alias, " on ",
                    OnClause
                ]
        end,
        <<>>,
        Joins).

make_sort(Sort, Q) ->
    lists:filtermap(
        fun
            ({edge, AscDesc, Term}) ->
                case first_edge_alias(Q) of
                    undefined ->
                        false;
                    Alias ->
                        {true, [ Alias, ".", Term, " ", ascdesc(AscDesc) ]}
                end;
            ({Alias, AscDesc, Term}) ->
                {true, [ Alias, ".", Term, " ", ascdesc(AscDesc) ]};
            (S) when is_binary(S); is_list(S) ->
                {true, S}
        end,
        Sort).

ascdesc($+) -> <<"ASC">>;
ascdesc($-) -> <<"DESC">>.

first_edge_alias(Q) ->
    case find_edge_alias(Q#search_sql_term.tables) of
        undefined ->
            case find_edge_alias(Q#search_sql_term.join_inner) of
                undefined ->
                    find_edge_alias(Q#search_sql_term.join_left);
                Alias ->
                    Alias
            end;
        Alias ->
            Alias
    end.

find_edge_alias(Map) when is_map(Map) ->
    maps:fold(
        fun
            (Alias, {<<"edge">>, _OnClause}, undefined) ->
                Alias;
            (Alias, {"edge", _OnClause}, undefined) ->
                Alias;
            (Alias, <<"edge">>, undefined) ->
                Alias;
            (Alias, "edge", undefined) ->
                Alias;
            (_, _, Found) ->
                Found
        end,
        undefined,
        Map).

%% @doc Compile nested boolean terms. Aliases which are only used below an
%% anyof or noneof boundary are kept inside the subquery.
compile_terms(Terms, AllAliases, OutsideAliases, Args, Context) ->
    compile_terms(Terms, AllAliases, OutsideAliases, #{}, Args, Context).

compile_terms([], _AllAliases, _OutsideAliases, _BeforeAliases, Args, _Context) ->
    {[], Args};
compile_terms([Term | Rest], AllAliases, OutsideAliases, BeforeAliases, Args0, Context) ->
    RestAliases = used_aliases(Rest, AllAliases),
    TermOutside = alias_union([OutsideAliases, BeforeAliases, RestAliases]),
    {Term1, Args1} = compile_term(Term, AllAliases, TermOutside, Args0, Context),
    BeforeAliases1 = alias_union(BeforeAliases, used_aliases(Term, AllAliases)),
    {Rest1, Args2} = compile_terms(
        Rest, AllAliases, OutsideAliases, BeforeAliases1, Args1, Context),
    {[Term1 | Rest1], Args2}.

compile_term(#search_sql_nested{ operator = {left_join, Alias}, terms = Terms },
        _AllAliases, OutsideAliases, Args0, Context) ->
    % Compile the right-hand side in its own scope. It can reference aliases
    % from the left, but none of its tables, filters, categories, or ACL checks
    % are allowed to escape into the outer query.
    InnerAliases = defined_aliases(Terms),
    InnerAllAliases = alias_union(InnerAliases, OutsideAliases),
    {Terms1, Args1} = compile_terms(
        Terms, InnerAllAliases, OutsideAliases, Args0, Context),
    Term0 = lists:foldr(
        fun merge_term/2,
        #search_sql_term{ select = [], tables = #{} },
        Terms1),
    {Subquery, Args2} = scoped_subquery(Term0, Args1, Context),
    {
        #search_sql_term{
            select = [],
            tables = #{},
            join_left = #{
                Alias => {
                    [<<"LATERAL (">>, Subquery, $)],
                    <<"true">>
                }
            }
        },
        Args2
    };
compile_term(#search_sql_nested{ operator = <<"allof">>, terms = Terms },
        AllAliases, OutsideAliases, Args0, Context) ->
    {Terms1, Args1} = compile_terms(Terms, AllAliases, OutsideAliases, Args0, Context),
    {combine_operator(<<"allof">>, Terms1), Args1};
compile_term(#search_sql_nested{ operator = <<"anyof">>, terms = Terms },
        AllAliases, OutsideAliases, Args0, Context) ->
    {Terms1, Args1} = compile_alternatives(
        Terms, AllAliases, OutsideAliases, Args0, Context),
    scope_term(
        combine_operator(<<"anyof">>, Terms1),
        OutsideAliases, exists, AllAliases, Args1, Context);
compile_term(#search_sql_nested{
        operator = <<"noneof">>,
        terms = [#search_sql_nested{ operator = <<"allof">> } = Term]
    }, AllAliases, OutsideAliases, Args0, Context) ->
    % Preserve a single conjunctive pattern until it is scoped, so it becomes
    % one NOT EXISTS subquery instead of NOT(EXISTS(...)).
    {Term1, Args1} = compile_term(
        Term, AllAliases, OutsideAliases, Args0, Context),
    scope_noneof(Term1, OutsideAliases, AllAliases, Args1, Context);
compile_term(#search_sql_nested{
        operator = <<"noneof">>,
        terms = [#search_sql_term{} = Term]
    }, AllAliases, OutsideAliases, Args0, Context) ->
    {Term1, Args1} = compile_term(Term, AllAliases, OutsideAliases, Args0, Context),
    scope_noneof(Term1, OutsideAliases, AllAliases, Args1, Context);
compile_term(#search_sql_nested{ operator = <<"noneof">>, terms = Terms },
        AllAliases, OutsideAliases, Args0, Context) ->
    {Terms1, Args1} = compile_alternatives(
        Terms, AllAliases, OutsideAliases, Args0, Context),
    scope_noneof(
        combine_operator(<<"anyof">>, Terms1),
        OutsideAliases, AllAliases, Args1, Context);
compile_term(#search_sql_term{} = Term, AllAliases, _OutsideAliases, Args, Context) ->
    map_sql_expressions(
        Term,
        fun(Exists, Terms, Args0) ->
            compile_exists_expression(Exists, Terms, AllAliases, Args0, Context)
        end,
        Args).

%% @doc A scalar EXISTS is a scope boundary, including when used in SELECT,
%% a function argument, or a compound condition. Its joins and checks must
%% never become requirements on the enclosing query.
compile_exists_expression(Exists, Terms, OuterAliases, Args0, Context) ->
    InnerAliases = defined_aliases(Terms),
    AllAliases = alias_union(InnerAliases, OuterAliases),
    {Terms1, Args1} = compile_terms(Terms, AllAliases, OuterAliases, Args0, Context),
    Term0 = lists:foldr(fun merge_term/2, #search_sql_term{ select = [], tables = #{} }, Terms1),
    {Subquery, Args2} = scoped_subquery(Term0#search_sql_term{ select = [<<"1">>] }, Args1, Context),
    {[exists_prefix(Exists), Subquery, $)], Args2}.

scoped_subquery(#search_sql_term{
        select = Select0,
        tables = Tables0,
        join_inner = JoinInner,
        join_left = JoinLeft,
        where = Where,
        cats = Cats,
        cats_exclude = CatsExclude,
        cats_exact = CatsExact,
        extra = Extra
    }, Args0, Context) ->
    % `rsc` is the implicit outer search resource. References to it in a
    % scoped subquery are correlations, not a new local table with the same alias.
    Tables = maps:remove(<<"rsc">>, Tables0),
    Select = case Select0 of
        [] -> [<<"1 AS optional_match">>];
        _ -> Select0
    end,
    {Where1, Args1} = add_local_sql_checks(
        Tables,
        JoinInner,
        JoinLeft,
        Where,
        Cats,
        CatsExclude,
        CatsExact,
        Extra,
        Args0,
        Context),
    {From, FromWhere} = subquery_from(Tables, JoinInner, JoinLeft),
    {
        [
            <<"SELECT ">>, lists:join(<<", ">>, Select),
            From,
            subquery_where(FromWhere, Where1)
        ],
        Args1
    }.

compile_alternatives(Terms, AllAliases, OutsideAliases, Args, Context) ->
    compile_alternatives(Terms, AllAliases, OutsideAliases, #{}, Args, Context).

compile_alternatives([], _AllAliases, _OutsideAliases, _BeforeAliases, Args, _Context) ->
    {[], Args};
compile_alternatives([Term | Rest], AllAliases, OutsideAliases, BeforeAliases, Args0, Context) ->
    RestAliases = used_aliases(Rest, AllAliases),
    TermOutside = alias_union([OutsideAliases, BeforeAliases, RestAliases]),
    {Term1, Args1} = compile_term(Term, AllAliases, TermOutside, Args0, Context),
    {Term2, Args2} = scope_term(
        Term1, TermOutside, exists, AllAliases, Args1, Context),
    BeforeAliases1 = alias_union(BeforeAliases, used_aliases(Term, AllAliases)),
    {Rest1, Args3} = compile_alternatives(
        Rest, AllAliases, OutsideAliases, BeforeAliases1, Args2, Context),
    {[Term2 | Rest1], Args3}.

combine_operator(Op, Terms) ->
    Conditions = [ where_expression(Term#search_sql_term.where) || Term <- Terms ],
    Acc0 = #search_sql_term{ select = [], tables = #{}, where = [] },
    Acc1 = lists:foldr(
        fun(Term, Acc) ->
            merge_term(Term#search_sql_term{ where = [] }, Acc)
        end,
        Acc0,
        Terms),
    Acc1#search_sql_term{
        where = [operator_expression(Op, Conditions)]
    }.

operator_expression(<<"allof">>, []) ->
    <<"true">>;
operator_expression(<<"anyof">>, []) ->
    <<"false">>;
operator_expression(Op, Conditions) ->
    [op_prefix(Op), lists:join(op(Op), Conditions), op_postfix(Op)].

where_expression([]) ->
    <<"true">>;
where_expression(Where) ->
    [<<"(">>, Where, <<")">>].

scope_noneof(Term, OutsideAliases, AllAliases, Args, Context) ->
    case local_aliases(Term, OutsideAliases, AllAliases) of
        LocalAliases when map_size(LocalAliases) =:= 0 ->
            {
                Term#search_sql_term{
                    where = [[<<"NOT ">>, where_expression(Term#search_sql_term.where)]]
                },
                Args
            };
        LocalAliases ->
            scope_local_term(Term, LocalAliases, not_exists, Args, Context)
    end.

scope_term(Term, OutsideAliases, Exists, AllAliases, Args, Context) ->
    case local_aliases(Term, OutsideAliases, AllAliases) of
        LocalAliases when map_size(LocalAliases) =:= 0 ->
            {Term, Args};
        LocalAliases ->
            scope_local_term(Term, LocalAliases, Exists, Args, Context)
    end.

scope_local_term(#search_sql_term{
        tables = Tables,
        join_inner = JoinInner,
        join_left = JoinLeft,
        where = Where,
        cats = Cats,
        cats_exclude = CatsExclude,
        cats_exact = CatsExact,
        extra = Extra
    } = Term, LocalAliases, Exists, Args0, Context) ->
    LocalTables = take_aliases(Tables, LocalAliases),
    LocalJoinInner = take_aliases(JoinInner, LocalAliases),
    LocalJoinLeft = take_aliases(JoinLeft, LocalAliases),
    LocalCats = take_alias_cats(Cats, LocalAliases),
    LocalCatsExclude = take_alias_cats(CatsExclude, LocalAliases),
    LocalCatsExact = take_alias_cats(CatsExact, LocalAliases),
    {Where1, Args1} = add_local_sql_checks(
        LocalTables,
        LocalJoinInner,
        LocalJoinLeft,
        Where,
        LocalCats,
        LocalCatsExclude,
        LocalCatsExact,
        Extra,
        Args0,
        Context),
    {
        Term#search_sql_term{
            tables = drop_aliases(Tables, LocalAliases),
            join_inner = drop_aliases(JoinInner, LocalAliases),
            join_left = drop_aliases(JoinLeft, LocalAliases),
            cats = drop_alias_cats(Cats, LocalAliases),
            cats_exclude = drop_alias_cats(CatsExclude, LocalAliases),
            cats_exact = drop_alias_cats(CatsExact, LocalAliases),
            where = [exists_expression(
                Exists, LocalTables, LocalJoinInner, LocalJoinLeft, Where1)]
        },
        Args1
    }.

add_local_sql_checks(_Tables, _JoinInner, _JoinLeft, Where,
        _Cats, _CatsExclude, _CatsExact, _Extra, Args, undefined) ->
    {merge_sql_checks(Where, []), Args};
add_local_sql_checks(Tables, JoinInner, JoinLeft, Where,
        Cats, CatsExclude, CatsExact, Extra, Args, Context) ->
    {From, _FromWhere} = subquery_from(Tables, JoinInner, JoinLeft),
    Query = #search_sql{
        select = <<"1">>,
        from = From,
        where = iolist_to_binary(Where),
        args = Args,
        tables = resource_tables(Tables, JoinInner, JoinLeft),
        cats = Cats,
        cats_exclude = CatsExclude,
        cats_exact = CatsExact,
        extra = Extra
    },
    {Checks, Args1} = z_search_acl:add_sql_checks(Query, Context),
    {merge_sql_checks(Where, Checks), Args1}.

merge_sql_checks(Where, Checks) ->
    Clauses = [
        Clause
        || Sql <- Where ++ Checks,
           Clause <- [iolist_to_binary(Sql)],
           Clause =/= <<>>
    ],
    lists:join(<<" AND ">>, [ [$(, Clause, $)] || Clause <- Clauses ]).

exists_expression(Exists, Tables, JoinInner, JoinLeft, Where) ->
    {From, FromWhere} = subquery_from(Tables, JoinInner, JoinLeft),
    [
        exists_prefix(Exists),
        <<"SELECT 1">>,
        From,
        subquery_where(FromWhere, Where),
        <<")">>
    ].

exists_prefix(exists) -> <<"EXISTS (">>;
exists_prefix(not_exists) -> <<"NOT EXISTS (">>.

subquery_from(Tables, JoinInner, JoinLeft) ->
    case take_first(Tables) of
        {Alias, Table, RestTables} ->
            {
                [
                    <<" FROM ">>, Table, <<" ">>, Alias,
                    make_cross_join(RestTables),
                    make_join(JoinInner, "join"),
                    make_join(JoinLeft, "left join")
                ],
                []
            };
        undefined ->
            subquery_from_join(JoinInner, JoinLeft)
    end.

subquery_from_join(JoinInner, JoinLeft) ->
    case take_first(JoinInner) of
        {Alias, {Table, OnClause}, RestJoinInner} ->
            {
                [
                    <<" FROM ">>, Table, <<" ">>, Alias,
                    make_join(RestJoinInner, "join"),
                    make_join(JoinLeft, "left join")
                ],
                OnClause
            };
        undefined when map_size(JoinLeft) =:= 0 ->
            {<<>>, []};
        undefined ->
            % A left join needs a left-hand relation. Join from a single row
            % if the subquery has no table or inner join from which we can
            % start.
            {
                [
                    <<" FROM (SELECT 1) AS z_search_subquery">>,
                    make_join(JoinLeft, "left join")
                ],
                []
            }
    end.

take_first(Map) ->
    case maps:to_list(Map) of
        [] ->
            undefined;
        [{Alias, Value} | _] ->
            {Alias, Value, maps:remove(Alias, Map)}
    end.

% Use a cross join instead of "from table1, table2, ..", as the
% cross join mixes nicer with the left/inner joins that are also
% produced.
make_cross_join(Tables) ->
    maps:fold(
        fun(Alias, Table, Acc) ->
            [Acc, <<" cross join ">>, Table, <<" ">>, Alias]
        end,
        <<>>,
        Tables).

subquery_where([], []) ->
    <<>>;
subquery_where(<<>>, []) ->
    <<>>;
subquery_where([], Where) ->
    [<<" where ">>, Where];
subquery_where(<<>>, Where) ->
    [<<" where ">>, Where];
subquery_where(FromWhere, []) ->
    [<<" where ">>, FromWhere];
subquery_where(FromWhere, <<>>) ->
    [<<" where ">>, FromWhere];
subquery_where(FromWhere, Where) ->
    [
        <<" where (">>, FromWhere,
        <<") AND (">>, Where,
        <<")">>
    ].

take_aliases(Map, Aliases) ->
    maps:filter(
        fun(Alias, _Value) -> maps:is_key(Alias, Aliases) end,
        Map).

take_alias_cats(Cats, Aliases) ->
    lists:filter(
        fun({Alias, _Categories}) ->
            maps:is_key(z_convert:to_binary(Alias), Aliases)
        end,
        Cats).

drop_aliases(Map, Aliases) ->
    maps:filter(
        fun(Alias, _Value) -> not maps:is_key(Alias, Aliases) end,
        Map).

drop_alias_cats(Cats, Aliases) ->
    lists:filter(
        fun({Alias, _Categories}) ->
            not maps:is_key(z_convert:to_binary(Alias), Aliases)
        end,
        Cats).

resource_tables(Tables, JoinInner, JoinLeft) ->
    lists:usort(
        resource_tables(Tables)
        ++ resource_join_tables(JoinInner)
        ++ resource_join_tables(JoinLeft)).

resource_tables(Tables) ->
    [
        {rsc, Alias}
        || {Alias, Table} <- maps:to_list(Tables),
           is_rsc_table(Table)
    ].

resource_join_tables(Joins) ->
    [
        {rsc, Alias}
        || {Alias, {Table, _OnClause}} <- maps:to_list(Joins),
           is_rsc_table(Table)
    ].

is_rsc_table(rsc) -> true;
is_rsc_table(<<"rsc">>) -> true;
is_rsc_table("rsc") -> true;
is_rsc_table(_Table) -> false.

merge_term(Term, Acc) ->
    #search_sql_term{
        select = Select,
        tables = Tables,
        join_left = JoinLeft,
        join_inner = JoinInner,
        where = Where,
        group_by = GroupBy,
        having = Having,
        sort = Sort,
        asort = ASort,
        zsort = ZSort,
        cats = Cats,
        cats_exact = CatsExact,
        cats_exclude = CatsExclude,
        extra = Extra
    } = Term,
    Acc#search_sql_term{
        select = merge_select(Acc#search_sql_term.select, Select),
        tables = maps:merge(Acc#search_sql_term.tables, Tables),
        join_left = maps:merge(Acc#search_sql_term.join_left, JoinLeft),
        join_inner = maps:merge(Acc#search_sql_term.join_inner, JoinInner),
        where = merge_where(Acc#search_sql_term.where, Where),
        group_by = Acc#search_sql_term.group_by ++ GroupBy,
        having = Acc#search_sql_term.having ++ Having,
        sort = Acc#search_sql_term.sort ++ Sort,
        asort = Acc#search_sql_term.asort ++ ASort,
        zsort = Acc#search_sql_term.zsort ++ ZSort,
        cats = cats(Acc#search_sql_term.cats, Cats),
        cats_exact = cats(Acc#search_sql_term.cats_exact, CatsExact),
        cats_exclude = cats(Acc#search_sql_term.cats_exclude, CatsExclude),
        extra = lists:usort(Acc#search_sql_term.extra ++ Extra)
    }.

op(<<"allof">>) -> <<" AND ">>;
op(<<"anyof">>) -> <<" OR ">>;
op(<<"noneof">>) -> <<" OR ">>.

op_prefix(<<"noneof">>) -> <<" NOT(">>;
op_prefix(_) -> <<"(">>.

op_postfix(_) -> <<")">>.


%% Alias scope analysis

defined_aliases(Terms) when is_list(Terms) ->
    lists:foldl(
        fun(Term, Acc) -> alias_union(Acc, defined_aliases(Term)) end,
        #{},
        Terms);
defined_aliases(#search_sql_nested{ operator = {left_join, Alias} }) ->
    alias_set([Alias]);
defined_aliases(#search_sql_nested{ terms = Terms }) ->
    defined_aliases(Terms);
defined_aliases(#search_sql_term{
        tables = Tables,
        join_inner = JoinInner,
        join_left = JoinLeft
    }) ->
    maps:remove(<<"rsc">>, alias_set(
        maps:keys(Tables) ++ maps:keys(JoinInner) ++ maps:keys(JoinLeft))).

used_aliases(Terms, AllAliases) when is_list(Terms) ->
    lists:foldl(
        fun(Term, Acc) -> alias_union(Acc, used_aliases(Term, AllAliases)) end,
        #{},
        Terms);
used_aliases(#search_sql_nested{
        operator = {left_join, Alias},
        terms = Terms
    }, AllAliases) ->
    % Internal aliases are private to the lateral subquery. Only its exported
    % alias and references to aliases defined outside count in the outer scope.
    alias_union(
        alias_set([Alias]),
        alias_intersection(used_aliases(Terms, AllAliases), AllAliases));
used_aliases(#search_sql_nested{ terms = Terms }, AllAliases) ->
    used_aliases(Terms, AllAliases);
used_aliases(#search_sql_term{} = Term, AllAliases) ->
    alias_union(defined_aliases(Term), referenced_aliases(Term, AllAliases)).

referenced_aliases(#search_sql_term{
        select = Select,
        join_inner = JoinInner,
        join_left = JoinLeft,
        where = Where,
        group_by = GroupBy,
        having = Having,
        sort = Sort,
        asort = ASort,
        zsort = ZSort,
        cats = Cats,
        cats_exclude = CatsExclude,
        cats_exact = CatsExact
    }, AllAliases) ->
    alias_union([
        aliases_in(Select, AllAliases),
        aliases_in_join(JoinInner, AllAliases),
        aliases_in_join(JoinLeft, AllAliases),
        aliases_in(Where, AllAliases),
        aliases_in(GroupBy, AllAliases),
        aliases_in(Having, AllAliases),
        aliases_in(Sort, AllAliases),
        aliases_in(ASort, AllAliases),
        aliases_in(ZSort, AllAliases),
        aliases_in(Cats, AllAliases),
        aliases_in(CatsExclude, AllAliases),
        aliases_in(CatsExact, AllAliases)
    ]).

exported_aliases(#search_sql_term{
        select = Select,
        group_by = GroupBy,
        having = Having,
        sort = Sort,
        asort = ASort,
        zsort = ZSort
    }, AllAliases) ->
    alias_union([
        aliases_in(Select, AllAliases),
        aliases_in(GroupBy, AllAliases),
        aliases_in(Having, AllAliases),
        aliases_in(Sort, AllAliases),
        aliases_in(ASort, AllAliases),
        aliases_in(ZSort, AllAliases)
    ]).

aliases_in_join(Joins, AllAliases) ->
    maps:fold(
        fun
            (_Alias, {_Table, OnClause}, Acc) ->
                alias_union(Acc, aliases_in(OnClause, AllAliases));
            (_Alias, _Table, Acc) ->
                Acc
        end,
        #{},
        Joins).

aliases_in(Value, AllAliases) ->
    maps:filter(
        fun(Alias, _True) -> mentions_alias(Value, Alias) end,
        AllAliases).

mentions_alias({search_sql_exists, _Exists, Terms}, Alias) ->
    % Only correlations count as references in the enclosing query.
    not maps:is_key(Alias, defined_aliases(Terms)) andalso mentions_alias(Terms, Alias);
mentions_alias(Alias, Alias) when is_binary(Alias) ->
    true;
mentions_alias(Value, Alias) when is_binary(Value) ->
    binary:match(Value, <<Alias/binary, ".">>) =/= nomatch;
mentions_alias(Value, Alias) when is_list(Value) ->
    lists:any(fun(V) -> mentions_alias(V, Alias) end, Value);
mentions_alias(Value, Alias) when is_tuple(Value) ->
    mentions_alias(tuple_to_list(Value), Alias);
mentions_alias(Value, Alias) when is_map(Value) ->
    lists:any(fun(V) -> mentions_alias(V, Alias) end, maps:values(Value));
mentions_alias(_Value, _Alias) ->
    false.

local_aliases(Term, OutsideAliases, AllAliases) ->
    Defined = defined_aliases(Term),
    Exported = exported_aliases(Term, AllAliases),
    Required0 = alias_intersection(Defined, alias_union(OutsideAliases, Exported)),
    Required = alias_dependencies(Required0, Defined, Term, AllAliases),
    alias_subtract(Defined, Required).

alias_dependencies(Required, Defined, Term, AllAliases) ->
    Dependencies = maps:fold(
        fun(Alias, _True, Acc) ->
            alias_union(Acc, alias_definition_dependencies(Alias, Term, AllAliases))
        end,
        #{},
        Required),
    Required1 = alias_union(Required, alias_intersection(Defined, Dependencies)),
    case map_size(Required1) =:= map_size(Required) of
        true -> Required;
        false -> alias_dependencies(Required1, Defined, Term, AllAliases)
    end.

alias_definition_dependencies(Alias, #search_sql_term{
        join_inner = JoinInner,
        join_left = JoinLeft
    }, AllAliases) ->
    alias_union([
        join_definition_dependencies(Alias, JoinInner, AllAliases),
        join_definition_dependencies(Alias, JoinLeft, AllAliases)
    ]).

join_definition_dependencies(Alias, Joins, AllAliases) ->
    case maps:find(Alias, Joins) of
        {ok, {_Table, OnClause}} -> aliases_in(OnClause, AllAliases);
        {ok, _Table} -> #{};
        error -> #{}
    end.

alias_set(Aliases) ->
    maps:from_list([ {Alias, true} || Alias <- Aliases ]).

alias_union(Aliases) when is_list(Aliases) ->
    lists:foldl(fun alias_union/2, #{}, Aliases).

alias_union(A, B) ->
    maps:merge(A, B).

alias_intersection(A, B) ->
    maps:filter(fun(Alias, _True) -> maps:is_key(Alias, B) end, A).

alias_subtract(A, B) ->
    maps:filter(fun(Alias, _True) -> not maps:is_key(Alias, B) end, A).


%% Query argument mapping

map_tree_args(Terms) ->
    map_tree_args(Terms, []).

map_tree_args([], Args) ->
    {[], Args};
map_tree_args([Term | Rest], Args0) ->
    {Rest1, Args1} = map_tree_args(Rest, Args0),
    {Term1, Args2} = map_tree_args(Term, Args1),
    {[Term1 | Rest1], Args2};
map_tree_args(#search_sql_nested{ terms = Terms } = Nested, Args0) ->
    {Terms1, Args1} = map_tree_args(Terms, Args0),
    {Nested#search_sql_nested{ terms = Terms1 }, Args1};
map_tree_args(#search_sql_term{ args = Args } = Term, Args0) ->
    {_, Args1, Mapping} = merge_args(Args, Args0),
    Term1 = map_args(Term, Mapping),
    map_sql_expressions(
        Term1#search_sql_term{ args = [] },
        fun
            (argument, Value, AccArgs) ->
                {_, NextArgs, ArgMapping} = merge_args([Value], AccArgs),
                {maps:get('$1', ArgMapping), NextArgs};
            (Exists, Terms, AccArgs) ->
                {Terms1, NextArgs} = map_tree_args(Terms, AccArgs),
                {{search_sql_exists, Exists, Terms1}, NextArgs}
        end,
        Args1).

%% SQL fragments may contain deferred scalar subqueries. Visit them before
%% rendering iodata, keeping their parameters separate from the containing
%% term until all parameters have been assigned query-wide positions.
map_sql_expressions(Term, Fun, Acc0) ->
    Fields = [Term#search_sql_term.select, Term#search_sql_term.where,
        Term#search_sql_term.group_by, Term#search_sql_term.having,
        Term#search_sql_term.sort, Term#search_sql_term.asort, Term#search_sql_term.zsort,
        Term#search_sql_term.join_inner, Term#search_sql_term.join_left],
    {[Select, Where, GroupBy, Having, Sort, ASort, ZSort, JoinInner, JoinLeft], Acc1} =
        map_sql_fragment(Fields, Fun, Acc0),
    {Term#search_sql_term{
        select = Select, where = Where, group_by = GroupBy, having = Having,
        sort = Sort, asort = ASort, zsort = ZSort,
        join_inner = JoinInner, join_left = JoinLeft
    }, Acc1}.

map_sql_fragment({search_sql_arg, Value}, Fun, Acc) ->
    Fun(argument, Value, Acc);
map_sql_fragment({search_sql_exists, Exists, Terms}, Fun, Acc) ->
    Fun(Exists, Terms, Acc);
map_sql_fragment(Values, Fun, Acc) when is_list(Values) ->
    lists:mapfoldl(fun(Value, A) -> map_sql_fragment(Value, Fun, A) end, Acc, Values);
map_sql_fragment(Values, Fun, Acc) when is_map(Values) ->
    {Pairs, Acc1} = map_sql_fragment(maps:to_list(Values), Fun, Acc),
    {maps:from_list(Pairs), Acc1};
map_sql_fragment(Values, Fun, Acc) when is_tuple(Values) ->
    {List, Acc1} = map_sql_fragment(tuple_to_list(Values), Fun, Acc),
    {list_to_tuple(List), Acc1};
map_sql_fragment(Value, _Fun, Acc) ->
    {Value, Acc}.


merge_select(SAcc, Select) ->
    Select2 = Select -- SAcc,
    SAcc ++ Select2.

merge_where(SAcc, []) ->
    SAcc;
merge_where(SAcc, <<>>) ->
    SAcc;
merge_where(SAcc, Where) ->
    case iolist_to_binary(Where) of
        <<>> -> SAcc;
        W1 -> SAcc ++ [ W1 ]
    end.

merge_args(#search_sql_term{ args = ArgsNew }, #search_sql_term{ args = ArgsAcc }) ->
    merge_args(ArgsNew, ArgsAcc);
merge_args(ArgsNew, ArgsAcc) ->
    lists:foldl(
        fun(Arg, {N, Acc, Map}) ->
            K = list_to_atom([ $$ | integer_to_list(N) ]),
            {Idx, Acc1} = case index(Arg, 1, Acc) of
                none ->
                    {length(Acc)+1, Acc ++ [ Arg ]};
                Found ->
                    {Found, Acc}
            end,
            NArg = iolist_to_binary([ $$, integer_to_list(Idx) ]),
            {N+1, Acc1, Map#{ K => NArg }}
        end,
        {1, ArgsAcc, #{}},
        ArgsNew).

map_args(Term, Mapping) ->
    Term#search_sql_term{
        select = map(Term#search_sql_term.select, Mapping),
        tables = map(Term#search_sql_term.tables, Mapping),
        join_inner = map(Term#search_sql_term.join_inner, Mapping),
        join_left = map(Term#search_sql_term.join_left, Mapping),
        where = map(Term#search_sql_term.where, Mapping),
        group_by = map(Term#search_sql_term.group_by, Mapping),
        having = map(Term#search_sql_term.having, Mapping),
        sort = map(Term#search_sql_term.sort, Mapping),
        asort = map(Term#search_sql_term.asort, Mapping),
        zsort = map(Term#search_sql_term.zsort, Mapping)
    }.

map(Field, Mapping) when is_map(Field) ->
    maps:fold(
        fun(K, V, Acc) ->
            Acc#{ K => map(V, Mapping) }
        end,
        #{},
        Field);
map(Field, Mapping) when is_list(Field) ->
    lists:map(
        fun(V) ->
            map_1(V, Mapping)
        end,
        Field);
map(Field, Mapping) ->
    map_1(Field, Mapping).

map_1(N, _) when is_integer(N) ->
    N;
map_1(<<>>, _) ->
    <<>>;
map_1(L, Mapping) when is_list(L) ->
    lists:map(fun(T) -> map_1(T, Mapping) end, L);
map_1(B, _Mapping) when is_binary(B) ->
    B;
map_1({search_sql_arg, _} = Argument, _Mapping) ->
    % Captured values have no dependency on the containing term's numbering.
    Argument;
map_1({search_sql_exists, _, _} = Expression, _Mapping) ->
    % These terms have their own argument lists, mapped by map_tree_args/2.
    Expression;
map_1({Alias, OnClause}, Mapping) ->
    {Alias, map_1(OnClause, Mapping)};
map_1({Alias, AscDesc, Sort}, Mapping) ->
    {Alias, AscDesc, map_1(Sort, Mapping)};
map_1(A, Mapping) when is_atom(A) ->
    maps:get(A, Mapping).

index(_, _, []) -> none;
index(A, N, [ A | _ ]) -> N;
index(A, N, [ _ | T ]) -> index(A, N+1, T).


%% Merge the {Alias, Categories} filter
cats(Q, Add) ->
    lists:foldl(
        fun
            ({Alias, Cs}, Acc) when is_list(Cs) ->
                Alias1 = z_convert:to_binary(Alias),
                lists:foldl(
                    fun(C, CAcc) ->
                        add_or_append(Alias1, C, CAcc)
                    end,
                    Acc,
                    Cs);
            ({Alias, C}, Acc) ->
                Alias1 = z_convert:to_binary(Alias),
                add_or_append(Alias1, C, Acc)
        end,
        Q,
        Add).

%% Add a value to a proplist. If it is already there, the value is
%% replaced by a list of values.
add_or_append(Key, Value, PropList) ->
    V = case is_list(Value) of
        true -> [ Value ];
        false -> Value
    end,
    case proplists:get_value(Key, PropList) of
        undefined ->
            [{Key, [V]} | PropList];
        Val when is_list(Val) ->
            [{Key, [V | Val]} | proplists:delete(Key, PropList)];
        Val ->
            [{Key, [V, Val]} | proplists:delete(Key, PropList)]
    end.
