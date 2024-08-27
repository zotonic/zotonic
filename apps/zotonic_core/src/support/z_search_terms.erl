%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021-2023 Marc Worrell
%% @doc Combine search terms into a sql search query.
%% @end

%% Copyright 2021-2023 Marc Worrell
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
    % All tables merged from the terms
    Q = lists:foldr(
        fun combine_acc/2,
        #search_sql_term{},
        Terms),
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

combine_acc(#search_sql_nested{ operator = Op, terms = Terms }, Acc) ->
    SubAcc = Acc#search_sql_term{ where = [] },
    Acc1 = lists:foldr(
        fun combine_acc/2,
        SubAcc,
        Terms),
    SubWhere = case Acc1#search_sql_term.where of
        <<>> ->
            <<>>;
        [] ->
            <<>>;
        List ->
            [
                op_prefix(Op),
                lists:join(op(Op), List),
                op_postfix(Op)
            ]
    end,
    Acc1#search_sql_term{
        where = merge_where(Acc#search_sql_term.where, SubWhere)
    };
combine_acc(Term, Acc) ->
    {_, NewArgs, Mapping} = merge_args(Term, Acc),
    Term1 = map_args(Term, Mapping),
    #search_sql_term{
        select = Select,
        tables = Tables,
        join_left = JoinLeft,
        join_inner = JoinInner,
        where = Where,
        sort = Sort,
        asort = ASort,
        zsort = ZSort,
        cats = Cats,
        cats_exact = CatsExact,
        cats_exclude = CatsExclude,
        extra = Extra
    } = Term1,
    Acc#search_sql_term{
        select = merge_select(Acc#search_sql_term.select, Select),
        tables = maps:merge(Acc#search_sql_term.tables, Tables),
        join_left = maps:merge(Acc#search_sql_term.join_left, JoinLeft),
        join_inner = maps:merge(Acc#search_sql_term.join_inner, JoinInner),
        where = merge_where(Acc#search_sql_term.where, Where),
        sort = Acc#search_sql_term.sort ++ Sort,
        asort = Acc#search_sql_term.asort ++ ASort,
        zsort = Acc#search_sql_term.zsort ++ ZSort,
        cats = cats(Acc#search_sql_term.cats, Cats),
        cats_exact = cats(Acc#search_sql_term.cats_exact, CatsExact),
        cats_exclude = cats(Acc#search_sql_term.cats_exclude, CatsExclude),
        extra = lists:usort(Acc#search_sql_term.extra ++ Extra),
        args = NewArgs
    }.

op(<<"allof">>) -> <<" AND ">>;
op(<<"anyof">>) -> <<" OR ">>;
op(<<"noneof">>) -> <<" OR ">>.

op_prefix(<<"noneof">>) -> <<" NOT(">>;
op_prefix(_) -> <<"(">>.

op_postfix(_) -> <<")">>.


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
