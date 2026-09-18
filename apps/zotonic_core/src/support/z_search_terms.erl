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

    merge_args/2,
    map/2
    ]).


-include_lib("../../include/zotonic.hrl").

combine(#search_sql_terms{ terms = Terms, post_func = PostFunc }) ->
    Q = combine(Terms),
    Q#search_sql{
        post_func = PostFunc
    };
combine(Terms) when is_list(Terms) ->
    % Map all arguments before splitting nested terms into subqueries. This
    % keeps the argument numbers unique across the complete query.
    {Terms1, Args} = map_tree_args(Terms),
    AllAliases = defined_aliases(Terms1),
    Terms2 = compile_terms(Terms1, AllAliases, #{}),
    Q0 = lists:foldr(fun merge_term/2, #search_sql_term{}, Terms2),
    Q1 = maybe_remove_default_select(Q0),
    Q = Q1#search_sql_term{ args = Args },
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
        order = iolist_to_binary(
                    lists:join(", ",   make_sort(Q#search_sql_term.asort, Q)
                                    ++ make_sort(Q#search_sql_term.sort, Q)
                                    ++ make_sort(Q#search_sql_term.zsort, Q))),
        args = Q#search_sql_term.args,
        tables = [
            {rsc, <<"rsc">>}
        ],
        cats = Q#search_sql_term.cats,
        cats_exact = Q#search_sql_term.cats_exact,
        cats_exclude = Q#search_sql_term.cats_exclude,
        extra = Q#search_sql_term.extra,
        search_sql_terms = Terms
    }.

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
compile_terms(Terms, AllAliases, OutsideAliases) ->
    compile_terms(Terms, AllAliases, OutsideAliases, #{}).

compile_terms([], _AllAliases, _OutsideAliases, _BeforeAliases) ->
    [];
compile_terms([Term | Rest], AllAliases, OutsideAliases, BeforeAliases) ->
    RestAliases = used_aliases(Rest, AllAliases),
    TermOutside = alias_union([OutsideAliases, BeforeAliases, RestAliases]),
    Term1 = compile_term(Term, AllAliases, TermOutside),
    BeforeAliases1 = alias_union(BeforeAliases, used_aliases(Term, AllAliases)),
    [Term1 | compile_terms(Rest, AllAliases, OutsideAliases, BeforeAliases1)].

compile_term(#search_sql_nested{ operator = <<"allof">>, terms = Terms }, AllAliases, OutsideAliases) ->
    Terms1 = compile_terms(Terms, AllAliases, OutsideAliases),
    combine_operator(<<"allof">>, Terms1);
compile_term(#search_sql_nested{ operator = <<"anyof">>, terms = Terms }, AllAliases, OutsideAliases) ->
    Terms1 = compile_alternatives(Terms, AllAliases, OutsideAliases),
    scope_term(combine_operator(<<"anyof">>, Terms1), OutsideAliases, exists, AllAliases);
compile_term(#search_sql_nested{ operator = <<"noneof">>, terms = Terms }, AllAliases, OutsideAliases) ->
    Terms1 = compile_alternatives(Terms, AllAliases, OutsideAliases),
    scope_noneof(combine_operator(<<"anyof">>, Terms1), OutsideAliases, AllAliases);
compile_term(#search_sql_term{} = Term, _AllAliases, _OutsideAliases) ->
    Term.

compile_alternatives(Terms, AllAliases, OutsideAliases) ->
    compile_alternatives(Terms, AllAliases, OutsideAliases, #{}).

compile_alternatives([], _AllAliases, _OutsideAliases, _BeforeAliases) ->
    [];
compile_alternatives([Term | Rest], AllAliases, OutsideAliases, BeforeAliases) ->
    RestAliases = used_aliases(Rest, AllAliases),
    TermOutside = alias_union([OutsideAliases, BeforeAliases, RestAliases]),
    Term1 = compile_term(Term, AllAliases, TermOutside),
    Term2 = scope_term(Term1, TermOutside, exists, AllAliases),
    BeforeAliases1 = alias_union(BeforeAliases, used_aliases(Term, AllAliases)),
    [Term2 | compile_alternatives(Rest, AllAliases, OutsideAliases, BeforeAliases1)].

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

scope_noneof(Term, OutsideAliases, AllAliases) ->
    case local_aliases(Term, OutsideAliases, AllAliases) of
        LocalAliases when map_size(LocalAliases) =:= 0 ->
            Term#search_sql_term{
                where = [[<<"NOT ">>, where_expression(Term#search_sql_term.where)]]
            };
        LocalAliases ->
            scope_term(Term, LocalAliases, not_exists)
    end.

scope_term(Term, OutsideAliases, Exists, AllAliases) ->
    case local_aliases(Term, OutsideAliases, AllAliases) of
        LocalAliases when map_size(LocalAliases) =:= 0 ->
            Term;
        LocalAliases ->
            scope_term(Term, LocalAliases, Exists)
    end.

scope_term(#search_sql_term{
        tables = Tables,
        join_inner = JoinInner,
        join_left = JoinLeft,
        where = Where,
        cats = Cats,
        cats_exclude = CatsExclude,
        cats_exact = CatsExact
    } = Term, LocalAliases, Exists) ->
    LocalTables = take_aliases(Tables, LocalAliases),
    LocalJoinInner = take_aliases(JoinInner, LocalAliases),
    LocalJoinLeft = take_aliases(JoinLeft, LocalAliases),
    % Category restrictions for local aliases belong to the subquery. Keep
    % them out of the flattened outer term; the original term tree is retained
    % in #search_sql.search_sql_terms for subquery reformatting.
    Term#search_sql_term{
        tables = drop_aliases(Tables, LocalAliases),
        join_inner = drop_aliases(JoinInner, LocalAliases),
        join_left = drop_aliases(JoinLeft, LocalAliases),
        cats = drop_alias_cats(Cats, LocalAliases),
        cats_exclude = drop_alias_cats(CatsExclude, LocalAliases),
        cats_exact = drop_alias_cats(CatsExact, LocalAliases),
        where = [exists_expression(Exists, LocalTables, LocalJoinInner, LocalJoinLeft, Where)]
    }.

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
    {Term1#search_sql_term{ args = [] }, Args1}.


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
