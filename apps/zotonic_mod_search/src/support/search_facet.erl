%% @copyright 2021-2022 Driebit BV
%% @doc Faceted search using a facet.tpl for definition and a
%% postgresql table for searches.

%% Copyright 2021-2022 Driebit BV
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

% with filtered as (
%     select rsc.id
%     from tables
%     where
%         [some search conditions]
% )
% --for each facet:
% select
%     'brand' as facet,
%     brand as value,
%     count(*) as count
% from
%     filtered
% group by
%     brand
% union
% select
%     'cool-tag' as facet,
%     'cool-tag'as value,
%     count(*) as count
% from
%     filtered
% where
%     cool_tag
% union
% ...


-module(search_facet).

-record(facet_def, {
    name :: binary(),
    block :: atom(),
    type :: facet_type(),
    is_range :: boolean()
    }).

-type facet_type() :: text
                    | fulltext
                    | integer
                    | float
                    | boolean
                    | datetime
                    | id
                    | ids
                    | list.
-type facet_def() :: #facet_def{}.


-export([
    facet_values/1,

    search_query_facets/3,
    search_query_subfacets/3,

    qterm/3,

    pivot_rsc/2,
    pivot_all/1,
    pivot_batch/2,
    ensure_table/1,
    is_table_ok/1,
    facet_def/2,
    template_facets/1,
    facet_table/1,
    create_table/1,
    recreate_table/1
    ]).

-define(TEXT_LENGTH, 80).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Return all found values (or min/max for all facets).
-spec facet_values(z:context()) -> {ok, map()} | {error, term()}.
facet_values(Context) ->
    case template_facets(Context) of
        {ok, Facets} ->
            FVs = lists:foldl(
                fun
                    (#facet_def{ type = Type, name = Name } = Facet, Acc) when
                            Type =:= list;
                            Type =:= ids ->
                        Col = <<"f_", Name/binary>>,
                        Q = <<"select distinct unnest(", Col/binary,") as colval, min(id) from search_facet ",
                              "where ", Col/binary, " is not null ",
                              "group by colval">>,
                        Rs = lists:sort(z_db:q(Q, Context)),
                        Rs1 = lists:map(
                            fun({V,Id}) ->
                                #{
                                    <<"value">> => V,
                                    <<"facet_id">> => Id
                                }
                            end,
                            Rs),
                        Rs2 = labels(Facet, Rs1, Context),
                        Acc#{
                            Name => #{
                                <<"type">> => <<"value">>,
                                <<"values">> => Rs2
                            }
                        };
                    (#facet_def{ name = Name, is_range = true }, Acc) ->
                        Col = <<"f_", Name/binary>>,
                        Q = <<"select min(", Col/binary,"), max(", Col/binary,
                              ") from search_facet">>,
                        {Min, Max} = z_db:q_row(Q, Context),
                        Acc#{
                            Name => #{
                                <<"type">> => <<"range">>,
                                <<"min">> => Min,
                                <<"max">> => Max
                            }
                        };
                    (#facet_def{ name = Name } = Facet, Acc) ->
                        Col = <<"f_", Name/binary>>,
                        Q = <<"select ", Col/binary,", min(id) from search_facet ",
                              "where ", Col/binary, " is not null ",
                              "group by ", Col/binary>>,
                        Rs = lists:sort(z_db:q(Q, Context)),
                        Rs1 = lists:map(
                            fun({V,Id}) ->
                                #{
                                    <<"value">> => V,
                                    <<"facet_id">> => Id
                                }
                            end,
                            Rs),
                        Rs2 = labels(Facet, Rs1, Context),
                        Acc#{
                            Name => #{
                                <<"type">> => <<"value">>,
                                <<"values">> => Rs2
                            }
                        }
                end,
                #{},
                Facets),
            {ok, FVs};
        {error, _} = Error ->
            Error
    end.

%% @doc Add facets to the result set using the query. The facets are calculated
%% using the query without facets, per facet a subselect is done to show the variations for
%% that facet with respect to the other facets.
-spec search_query_facets(Result, Query, Context) -> NewResult
    when Result :: #search_result{},
         NewResult :: #search_result{},
         Query :: #search_sql{},
         Context :: z:context().
search_query_facets(Result, #search_sql{ search_sql_terms = undefined } = Query, Context) ->
    search_query_subfacets(Result, Query, Context);
search_query_facets(Result, #search_sql{ search_sql_terms = Terms }, Context) ->
    % 1. Take the query terms
    % 2. Split into facet terms and other terms
    {FacetTerms, SelectTerms} = lists:partition(fun is_facet_term/1, Terms),
    % 3. Rebuild the query for the other terms
    SelectTerms1 = [
        #search_sql_term{
            select = [ <<"rsc.id">>, <<"facet.id as facet_id">>, <<"facet.*">> ],
            join_inner = #{
                <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
            }
        }
        | SelectTerms
    ],

    Q1 = z_search_terms:combine(SelectTerms1),
    Q2 = Q1#search_sql{
        limit = undefined
    },
    Q3 = move_unused_order_args_to_select(Q2),
    Q4 = z_search:reformat_sql_query(Q3, Context),
    {SQL, Args} = z_search:concat_sql_query(Q4, undefined),
    SQL2 = [
        "with result as (", SQL, ")"
    ],
    {ok, Defs} = template_facets(Context),
    % 4. Add facet where clauses to all facet unions.
    {Unions, Args2} = lists:foldl(
        fun(Def, {UnionAcc, ArgsAcc}) ->
            {UnionTerm, ArgsAcc1} = facet_union(Def, FacetTerms, ArgsAcc),
            {[ UnionTerm | UnionAcc ], ArgsAcc1}
        end,
        {[], Args},
        Defs),
    SQL3 = [
        SQL2,
        " ",
        lists:join("\nunion\n", Unions)
    ],
    FinalSQL = iolist_to_binary(SQL3),
    {ok, Facets} = z_db:qmap(FinalSQL, Args2, Context),
    Fs = group_facets(Defs, Facets, Context),
    Result#search_result{
        facets = Fs
    }.

is_facet_term(#search_sql_term{ label = {facet, _} }) -> true;
is_facet_term(#search_sql_term{}) -> false.

%% @doc Add facets to the result set using the query. The facets are calculated
%% using the result ids. Facets can be used for a "drill down".
-spec search_query_subfacets(Result, Query, Context) -> NewResult
    when Result :: #search_result{},
         NewResult :: #search_result{},
         Query :: #search_sql{},
         Context :: z:context().
search_query_subfacets(Result, Query, Context) ->
    Q1 = join_facet(Query),
    Q2 = Q1#search_sql{
        select = "rsc.id, facet.id as facet_id, facet.*",
        limit = undefined
    },
    Q3 = move_unused_order_args_to_select(Q2),
    {SQL, Args} = z_search:concat_sql_query(Q3, undefined),
    SQL2 = [
        "with result as (", SQL, ")"
    ],
    {ok, Defs} = template_facets(Context),
    Unions = lists:map(fun facet_union/1, Defs),
    Unions1 = lists:filter(fun(X) -> X =/= [] end, Unions),
    SQL3 = [
        SQL2,
        " ",
        lists:join("\nunion\n", Unions1)
    ],
    FinalSQL = iolist_to_binary(SQL3),
    {ok, Facets} = z_db:qmap(FinalSQL, Args, Context),
    Fs = group_facets(Defs, Facets, Context),
    NewTotal = facet_total(Fs, Result#search_result.total),
    PageLen = Result#search_result.pagelen,
    Result#search_result{
        facets = Fs,
        total = NewTotal,
        pages = (NewTotal + PageLen - 1) div PageLen
    }.

join_facet(#search_sql{ from = From } = Query) ->
    case string:find(From, "search_facet facet") of
        nomatch ->
            From1 = iolist_to_binary([ From, ", search_facet facet "]),
            add_where("facet.id = rsc.id", "AND", Query#search_sql{ from = From1 });
        _ ->
            Query
    end.

add_where(Clause, _AndOr, #search_sql{ where = "" } = Search) ->
    Search#search_sql{ where = Clause };
add_where(Clause, _AndOr, #search_sql{ where = <<>> } = Search) ->
    Search#search_sql{ where = Clause };
add_where(Clause, AndOr, #search_sql{ where = C } = Search) ->
    W = iolist_to_binary([C, " ", AndOr, " ", Clause]),
    Search#search_sql{ where = W }.

move_unused_order_args_to_select(#search_sql{ where = Where, order = Order } = Q) ->
    InWhere = case re:run(Where, <<"\\$[0-9]+">>, [ global, {capture, all, binary} ]) of
        {match, Ws} -> lists:flatten(Ws);
        nomatch -> []
    end,
    InOrder = case re:run(Order, <<"\\$[0-9]+">>, [ global, {capture, all, binary} ]) of
        {match, Os} -> lists:flatten(Os);
        nomatch -> []
    end,
    Q1 = lists:foldl(
        fun(Ex, QAcc) ->
            QAcc#search_sql{
                select = iolist_to_binary([
                            QAcc#search_sql.select
                            ,", ", z_convert:to_binary(Ex), "::character varying"
                        ])
                }
        end,
        Q,
        InOrder -- InWhere),
    Q1#search_sql{ order = <<>> }.


facet_total(Fs, Total) ->
    maps:fold(
        fun
            (_, #{ <<"facet_type">> := <<"ids">> }, T) ->
                T;
            (_, #{ <<"facet_type">> := <<"list">> }, T) ->
                T;
            (_, #{ <<"counts">> := [] }, T) ->
                T;
            (_, #{ <<"type">> := <<"count">>, <<"counts">> := L }, T) when is_list(L) ->
                Ct = [ maps:get(<<"count">>, F) || F <- L ],
                erlang:max(T, lists:sum(Ct));
            (_, _, T) ->
                T
        end,
        Total,
        Fs).


group_facets(Defs, Facets, Context) ->
    % Set sudo permission to fetch facet values.
    % There has already been an ACL check on the returned ids, but some facet values
    % might be shared with invisible ids, which will then give problems when rendering
    % the specific value.
    ContextSudo = z_acl:sudo(Context),
    lists:foldl(
        fun
            (#facet_def{ name = Name, is_range = true, type = Type }, Acc) ->
                % value is min, count is max
                [ #{ <<"value">> := Min, <<"count">> := Max } ] = find_facets(Name, Facets),
                Acc#{
                    Name => #{
                        <<"facet_type">> => z_convert:to_binary(Type),
                        <<"type">> => <<"range">>,
                        <<"min">> => convert_single_type(Type, Min, Context),
                        <<"max">> => convert_single_type(Type, Max, Context)
                    }
                };
            (#facet_def{ name = Name, type = Type } = Facet, Acc) ->
                Fs = find_facets(Name, Facets),
                Vs = lists:map(
                    fun(#{ <<"value">> := V, <<"count">> := Ct } = F) ->
                        {binary_to_integer(Ct), V, F}
                    end,
                    Fs),
                Vs1 = lists:reverse( lists:sort(Vs) ),
                Vs2 = lists:map(
                    fun
                        ({Ct, V, F}) when Type =:= ids ->
                            F#{
                                <<"value">> => convert_single_type(id, V, Context),
                                <<"count">> => Ct
                            };
                        ({Ct, V, F}) when Type =:= list ->
                            F#{
                                <<"value">> => V,
                                <<"count">> => Ct
                            };
                        ({Ct, V, F}) ->
                            F#{
                                <<"value">> => convert_single_type(Type, V, Context),
                                <<"count">> => Ct
                            }
                    end,
                    Vs1),
                Vs3 = labels(Facet, Vs2, ContextSudo),
                Acc#{
                    Name => #{
                        <<"facet_type">> => z_convert:to_binary(Type),
                        <<"type">> => <<"count">>,
                        <<"counts">> => Vs3
                    }
                }
        end,
        #{},
        Defs).

find_facets(Name, Fs) ->
    lists:filter(
        fun(#{ <<"facet">> := N }) -> Name =:= N end,
        Fs).

facet_union(#facet_def{ name = Name } = Def, FacetTerms, Args) ->
    Frag = facet_union(Def),
    % Append all facet clauses for facets other
    % than the current facet def.
    FacetTerms1 = lists:filter(
        fun(#search_sql_term{ label = {facet, N} }) ->
            Name =/= N
        end,
        FacetTerms),

    % Update all arg placeholders in the where clause
    {Ws, Args1} = lists:foldl(
        fun(#search_sql_term{ where = TWhere, args = TArgs }, {WAcc, ArgsAcc}) ->
            {_, ArgsAcc1, Mapping} = z_search_terms:merge_args(TArgs, ArgsAcc),
            TWhere1 = z_search_terms:map(TWhere, Mapping),
            {[ TWhere1 | WAcc ], ArgsAcc1}
        end,
        {[], Args},
        FacetTerms1),
    Frag1 = case Ws of
        [] ->
            Frag;
        _ ->
            F1 = iolist_to_binary(Frag),
            Ws1 = iolist_to_binary([ " ", lists:join(<<" and ">>, Ws) ]),
            Ws2 = binary:replace(Ws1, <<" facet.">>, <<" ">>, [ global ]),
            Ws3 = binary:replace(Ws2, <<"(facet.">>, <<"(">>, [ global ]),
            Ws4 = <<" where ", Ws3/binary, " and ">>,
            binary:replace(F1, <<" where ">>, Ws4)
    end,
    {Frag1, Args1}.

facet_union(#facet_def{ type = Type, name = Name }) when
        Type =:= list;
        Type =:= ids ->
    Col = <<"f_", Name/binary>>,
    [
        "select '", Name, "' as facet,
            unnest(", Col, ")::character varying as value,
            min(facet_id)::integer as facet_id,
            count(*)::character varying as count
         from result
         where ", Col, " is not null
         group by value"
    ];
facet_union(#facet_def{ name = Name, is_range = true }) ->
    Col = <<"f_", Name/binary>>,
    [
        "select '", Name, "' as facet,
            min(", Col, ")::character varying as value,
            0 as facet_id,
            max(", Col, ")::character varying as count
         from result
         where ", Col, " is not null "
    ];
facet_union(#facet_def{ name = Name }) ->
    Col = <<"f_", Name/binary>>,
    [
        "select '", Name, "' as facet,
            ", Col ,"::character varying as value,
            min(facet_id)::integer as facet_id,
            count(*)::character varying as count
        from result
        where ", Col, " is not null
        group by ", Col
    ].


%% @doc Add an extra search argument to the given query. Called by the query
%% builder in search_query.erl
-spec qterm(Field, Value, Context) -> {ok, Term} | {error, term()}
    when Field :: binary(),
         Value :: term(),
         Term :: #search_sql_term{},
         Context :: z:context().
qterm(_Field, [], _Context) ->
    {ok, []};
qterm(Field, [Value], Context) ->
    Q = #search_sql_term{
        label = {facet, Field},
        join_inner = #{
            <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
        }
    },
    qterm_1(Field, Value, Q, Context);
qterm(Field, Vs, Context) when is_list(Vs) ->
    % 'OR' query for all values
    Q = #search_sql_term{
        label = {facet, Field},
        join_inner = #{
            <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
        }
    },
    Q2 = lists:foldl(
        fun(V, QAcc) ->
            case qterm_1(Field, V, QAcc, Context) of
                {ok, QAcc1} ->
                    QAcc1;
                {error, _} ->
                    QAcc
            end
        end,
        Q,
        Vs),
    Q3 = Q2#search_sql_term{
        where = [
            <<"(">>,
            lists:join(<<" OR ">>, Q2#search_sql_term.where),
            <<")">>
        ]
    },
    {ok, Q3};
qterm(Field, Value, Context) ->
    Q = #search_sql_term{
        join_inner = #{
            <<"facet">> => {<<"search_facet">>, <<"facet.id = rsc.id">>}
        }
    },
    qterm_1(Field, Value, Q, Context).

qterm_1(Field, Value, Query, Context) ->
    case facet_def(Field, Context) of
        {ok, Def} ->
            {Op, Value1} = extract_op(Value),
            Value2 = convert_type(Def#facet_def.type, Value1, Context),
            Final = case Def#facet_def.type of
                fulltext when Op =:= "=" ->
                    NormV = z_string:normalize(Value2),
                    {ArgN, Query2} = add_term_arg(<<"%", NormV/binary, "%">>, Query),
                    W = [
                        <<"facet.ft_">>, Field, <<" like ">>, ArgN
                    ],
                    Query2#search_sql_term{
                        label = {facet_ft, Field},
                        where = Query2#search_sql_term.where ++ [ W ]
                    };
                Array when Array =:= ids; Array =:= list ->
                    {ArgN, Query2} = add_term_arg(Value2, Query),
                    W = [
                        <<"facet.f_">>, Field, "@>", ArgN
                    ],
                    Query2#search_sql_term{
                        label = {facet, Field},
                        where = Query2#search_sql_term.where ++ [ W ]
                    };
                _ ->
                    {ArgN, Query2} = add_term_arg(Value2, Query),
                    W = [
                        <<"facet.f_">>, Field, Op, ArgN
                    ],
                    Query2#search_sql_term{
                        label = {facet, Field},
                        where = Query2#search_sql_term.where ++ [ W ]
                    }
            end,
            {ok, Final};
        {error, _} = Error ->
            ?LOG_NOTICE(#{
                text => <<"Unknown facet, dropping query term.">>,
                in => zotonic_mod_search,
                facet => Field
            }),
            Error
    end.

add_term_arg(ArgValue, #search_sql_term{ args = Args } = Q) ->
    Arg = [$$] ++ integer_to_list(length(Args) + 1),
    {list_to_atom(Arg), Q#search_sql_term{args = Args ++ [ ArgValue ]}}.

extract_op(<<"=", V/binary>>) ->
    {"=", V};
extract_op(<<">", V/binary>>) ->
    {">", V};
extract_op(<<"<", V/binary>>) ->
    {"<", V};
extract_op(<<"<=", V/binary>>) ->
    {"<=", V};
extract_op(<<">=", V/binary>>) ->
    {">=", V};
extract_op(<<"!=", V/binary>>) ->
    {"<>", V};
extract_op(<<"<>", V/binary>>) ->
    {"<>", V};
extract_op(V) ->
    {"=", V}.


% %% Append an argument to a #search_sql
% add_arg(ArgValue, Search) ->
%     Arg = [$$] ++ integer_to_list(length(Search#search_sql.args) + 1),
%     {Arg, Search#search_sql{args=Search#search_sql.args ++ [ArgValue]}}.


%% @doc Pivot all resources to fill the facet table. This runs after every change to the
%% the facet.tpl blocks.
-spec pivot_all( z:context() ) -> ok.
pivot_all(Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_search,
        text => <<"Faceted search: repivoting facet for all resources">>
    }),
    {ok, _} = z_pivot_rsc:insert_task_after(1, ?MODULE, pivot_batch, facet_pivot_batch, [0], Context),
    ok.

%% @doc Batch for running the facet table updates. This updates the table with 1000 resources
%% at a time.
pivot_batch(FromId, Context0) ->
    Context = z_acl:sudo(Context0),
    case z_db:q("select id from rsc where id > $1 order by id limit 1000", [FromId], Context) of
        [] ->
            done;
        Rs ->
            lists:foreach(
                fun({Id}) ->
                    pivot_rsc(Id, Context)
                end,
                Rs),
            {Max} = lists:last(Rs),
            {delay, 1, [Max]}
    end.

%% @doc Pivot a resource, fill the facet table.
-spec pivot_rsc( m_rsc:resource_id(), z:context() ) -> ok | {error, term()}.
pivot_rsc(Id, Context) ->
    case ensure_table(Context) of
        ok ->
            {ok, Facets} = template_facets(Context),
            Upd = maps:from_list(
                lists:flatten(
                    lists:map( fun(F) -> render_facet(Id, F, Context) end, Facets ) ) ),
            R = case z_db:q1("select id from search_facet where id = $1", [ Id ], Context) of
                undefined ->
                    Upd1 = Upd#{ <<"id">> => Id },
                    z_db:insert(search_facet, Upd1, Context);
                _ ->
                    z_db:update(search_facet, Id, Upd, Context)
            end,
            case R of
                {ok, _} ->
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

render_facet(Id, #facet_def{ name = Name, type = fulltext } = F, Context) ->
    case render_block(F#facet_def.block, {cat, <<"pivot/facet.tpl">>}, #{ <<"id">> => Id }, Context) of
        <<>> ->
            [];
        V ->
            [
                {<<"f_", Name/binary>>, z_string:truncatechars(V, ?TEXT_LENGTH)},
                {<<"ft_", Name/binary>>, z_string:truncatechars(z_string:normalize(V), ?TEXT_LENGTH)}
            ]
    end;
render_facet(Id, #facet_def{ name = Name, type = Type } = F, Context) ->
    V = render_block(F#facet_def.block, {cat, <<"pivot/facet.tpl">>}, #{ <<"id">> => Id }, Context),
    {<<"f_", Name/binary>>, convert_type(Type, V, Context)}.


render_block(Block, Template, Vars, Context) ->
    {Output, _RenderState} = z_template:render_block_to_iolist(Block, Template, Vars, Context),
    z_string:trim(iolist_to_binary(Output)).

%% @doc Convert a value into a type ok for the search query args. Array values are not
%% used here, only single valued types.
convert_single_type(Type, L, Context) when
        is_list(L),
        (Type =:= list orelse Type =:= ids) ->
    case convert_type(Type, L, Context) of
        [V|_] -> V;
        [] -> undefined
    end;
convert_single_type(ids, V, Context) ->
    convert_type(id, V, Context);
convert_single_type(list, V, Context) ->
    convert_type(text, V, Context);
convert_single_type(Type, V, Context) ->
    convert_type(Type, V, Context).

convert_type(Type, V, Context) ->
    try
        convert_type_1(Type, V, Context)
    catch
        T:E ->
            ?LOG_INFO(#{
                text => <<"Illegal facet search value for type">>,
                result => T,
                reason => E,
                value => V,
                type => Type,
                in => mod_search
            }),
            undefined
    end.

convert_type_1(list, [], _Context) ->
    undefined;
convert_type_1(list, L, _Context) when is_list(L) ->
    L1 = lists:map(fun z_convert:to_binary/1, L),
    L2 = lists:map(fun z_string:trim/1, L1),
    lists:filter( fun(B) -> B =/= <<>> end, L2 );
convert_type_1(ids, [], _Context) ->
    undefined;
convert_type_1(ids, L, Context) when is_list(L) ->
    lists:map(fun(V) -> convert_type_1(id, V, Context) end, L);
convert_type_1(Type, L, Context) when is_list(L) ->
    L1 = lists:map(fun(V) -> convert_type_1(Type, V, Context) end, L),
    lists:filter(fun(V) -> V =/= <<>> andalso V =/= undefined end, L1);
convert_type_1(boolean, V, _Context) -> z_convert:to_bool(V);
convert_type_1(_, <<>>, _Context) -> undefined;
convert_type_1(_, undefined, _Context) -> undefined;
convert_type_1(id, V, Context) -> m_rsc:rid(V, Context);
convert_type_1(integer, V, _Context) -> z_convert:to_integer(V);
convert_type_1(float, V, _Context) -> z_convert:to_float(V);
convert_type_1(datetime, V, _Context) -> z_datetime:to_datetime(V);
convert_type_1(list, V, Context) when is_binary(V) ->
    L = binary:split(V, <<"||">>, [ global ]),
    L1 = lists:map(fun z_string:trim/1, L),
    convert_type_1(list, lists:filter( fun(B) -> B =/= <<>> end, L1 ), Context);
convert_type_1(ids, V, Context) when is_binary(V) ->
    L = binary:split(V, <<"||">>, [ global ]),
    L1 = lists:map(fun z_string:trim/1, L),
    convert_type_1(ids, lists:filter( fun(B) -> B =/= <<>> end, L1 ), Context);
convert_type_1(fulltext, V, _Context) ->
    z_string:truncatechars(z_convert:to_binary(V), ?TEXT_LENGTH);
convert_type_1(text, V, _Context) ->
    z_string:truncatechars(z_convert:to_binary(V), ?TEXT_LENGTH).


%% @doc Ensure that the facet table is correct, if not then drop the existing
%% table and request a pivot of all resources to fill the table.
-spec ensure_table(z:context()) -> ok | {error, term()}.
ensure_table(Context) ->
    case is_table_ok(Context) of
        true ->
            ok;
        false ->
            case recreate_table(Context) of
                ok ->
                    pivot_all(Context),
                    ok;
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Check if the current table is compatible with the facets in pivot.tpl
-spec is_table_ok(z:context()) -> boolean().
is_table_ok(Context) ->
    DbCols = lists:filter(
        fun
            (#column_def{ name = id }) -> false;
            (#column_def{ name = _ }) -> true
        end,
        z_db:columns(search_facet, Context)),
    case facet_table(Context) of
        {ok, {TplCols, _}} when length(DbCols) =:= length(TplCols) ->
            lists:all(
                fun(#column_def{ name = Name, type = Type, is_array = IsArray }) ->
                    is_type(DbCols, Name, Type, IsArray)
                end,
                TplCols);
        {ok, _} ->
            false;
        {error, _} ->
            false
    end.

is_type([], _Name, _Type, _IsArray) ->
    false;
is_type([ #column_def{ name = Name, type = T, is_array = IsArray } | _ ], Name, Type, IsArray) ->
    z_convert:to_binary(T) =:= Type;
is_type([ _ | Cols ], Name, Type, IsArray) ->
    is_type(Cols, Name, Type, IsArray).


%% @doc Add label values to the fetched facets for faceted search
labels(_, [], _Context) ->
    [];
labels(Facet, Vs, Context) ->
    Vs1 = case has_label_block(Facet, Context) of
        {true, LabelBlock} ->
            value_via_block(LabelBlock, Vs, Context);
        false ->
            case Facet#facet_def.type of
                id ->
                    ids_as_labels(Vs, Context);
                ids ->
                    ids_as_labels(Vs, Context);
                _ ->
                    values_as_labels(Vs)
            end
    end,
    lists:map(fun(V) -> maps:remove(<<"facet_id">>, V) end, Vs1).

value_via_block(LabelBlock, Vs, Context) ->
    lists:map(
        fun
            (#{ <<"value">> := V, <<"facet_id">> := 0 } = F) ->
                F#{ <<"label">> => V };
            (#{ <<"value">> := V, <<"facet_id">> := Id } = F) ->
                % NOTA BENE:
                % The found id might not be visible.
                case render_block(LabelBlock, {cat, <<"pivot/facet.tpl">>}, #{ <<"id">> => Id }, Context) of
                    <<>> ->
                        F#{ <<"label">> => V };
                    T ->
                        F#{ <<"label">> => T }
                end
        end,
        Vs).

ids_as_labels(Vs, Context) ->
    lists:map(
        fun(#{ <<"value">> := Id } = F) ->
            T = case m_rsc:is_a(Id, person, Context) of
                true ->
                    {Name, _} = z_template:render_to_iolist("_name.tpl", #{ <<"id">> => Id }, Context),
                    iolist_to_binary(Name);
                false ->
                    m_rsc:p(Id, <<"title">>, Context)
            end,
            T1 = case z_utils:is_empty(T) of
                true -> m_rsc:p(Id, <<"short_title">>, Context);
                false -> T
            end,
            F#{ <<"label">> => z_trans:lookup_fallback(T1, Context) }
        end,
        Vs).

values_as_labels(Vs) ->
    lists:map(
        fun(#{ <<"value">> := V } = F) ->
            F#{ <<"label">> => V }
        end,
        Vs).

has_label_block(#facet_def{ block = Block }, Context) ->
    {ok, Blocks} = z_template:blocks(<<"pivot/facet.tpl">>, #{}, Context),
    B = atom_to_binary(Block, utf8),
    LabelBlock = binary_to_atom( <<"label_", B/binary>>, utf8 ),
    case lists:member(LabelBlock, Blocks) of
        true ->
            {true, LabelBlock};
        false ->
            false
    end.

%% @doc Recreate the facet table by first dropping it.
-spec recreate_table( z:context() ) -> ok | {error, term()}.
recreate_table(Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_search,
        text => <<"Faceted search: recreating facet table">>
    }),
    z_db:q("drop table if exists search_facet cascade", Context),
    z_db:flush(Context),
    create_table(Context).


%% @doc Generate the table for the facet pivoting. The definition of the
%% columns is derived from the facet.tpl template.
-spec create_table( z:context() ) -> ok | {error, term()}.
create_table(Context) ->
    case facet_table(Context) of
        {ok, {Cols, Idxs}} ->
            Cols1 = [
                #column_def{
                    name = id,
                    type = "integer",
                    length = undefined,
                    is_nullable = false,
                    default = undefined,
                    primary_key = true,
                    unique = true
                }
                | Cols
            ],
            ok = z_db:create_table(search_facet, Cols1, Context),
            [] = z_db:q(
                "ALTER TABLE search_facet ADD CONSTRAINT fk_facet_id FOREIGN KEY (id)
                 REFERENCES rsc (id)
                 ON UPDATE CASCADE ON DELETE CASCADE",
                Context),
            lists:foreach(
                fun(Idx) ->
                    [] = z_db:q(Idx, Context)
                end,
                Idxs),
            ok;
        {error, _} = Error ->
            Error
    end.


%% @doc Generate the face table definition from the facets.
-spec facet_table(z:context()) -> {ok, term()} | {error, term()}.
facet_table(Context) ->
    case template_facets(Context) of
        {ok, Facets} ->
            {Cs, Is} = lists:foldl(
                fun(Facet, {ColAcc, IdxAcc}) ->
                    Col = facet_to_column(Facet),
                    Idx = facet_to_index(Facet),
                    {[Col|ColAcc], [Idx|IdxAcc]}
                end,
                {[], []},
                Facets),
            Cs1 = lists:sort( lists:flatten(Cs) ),
            Is1 = lists:sort( lists:flatten(Is) ),
            {ok, {Cs1, Is1}};
        {error, _} = Error ->
            Error
    end.

facet_to_column(#facet_def{
        name = Name,
        type = fulltext
    }) ->
    [
        #column_def{
            name = binary_to_atom(<<"f_", Name/binary>>, utf8),
            type = col_type(text),
            length = col_length(text),
            is_nullable = true,
            default = undefined,
            primary_key = false,
            unique = false
        },
        #column_def{
            name = binary_to_atom(<<"ft_", Name/binary>>, utf8),
            type = <<"text">>,
            length = undefined,
            is_nullable = true,
            default = undefined,
            primary_key = false,
            unique = false
        }
    ];
facet_to_column(#facet_def{
        name = Name,
        type = list
    }) ->
    #column_def{
        name = binary_to_atom(<<"f_", Name/binary>>, utf8),
        type = <<"character varying">>,
        length = undefined,
        is_nullable = true,
        is_array = true,
        default = undefined,
        primary_key = false,
        unique = false
    };
facet_to_column(#facet_def{
        name = Name,
        type = ids
    }) ->
    #column_def{
        name = binary_to_atom(<<"f_", Name/binary>>, utf8),
        type = col_type(id),
        length = col_length(id),
        is_nullable = true,
        is_array = true,
        default = undefined,
        primary_key = false,
        unique = false
    };
facet_to_column(#facet_def{
        name = Name,
        type = Type
    }) ->
    #column_def{
        name = binary_to_atom(<<"f_", Name/binary>>, utf8),
        type = col_type(Type),
        length = col_length(Type),
        is_nullable = true,
        default = undefined,
        primary_key = false,
        unique = false
    }.


col_type(text) -> <<"character varying">>;
col_type(integer) -> <<"integer">>;
col_type(float) -> <<"double precision">>;
col_type(boolean) -> <<"boolean">>;
col_type(datetime) -> <<"timestamp with time zone">>;
col_type(id) -> <<"integer">>.

col_length(text) -> ?TEXT_LENGTH;
col_length(integer) -> undefined;
col_length(float) -> undefined;
col_length(boolean) -> undefined;
col_length(datetime) -> undefined;
col_length(id) -> undefined.


facet_to_index(#facet_def{
        name = Name,
        type = fulltext
    }) ->
    [
        <<"CREATE INDEX search_facet_f_", Name/binary, "_key ",
          "ON search_facet(f_", Name/binary, ")">>,

        <<"CREATE INDEX search_facet_ft_", Name/binary, "_key ",
           "ON search_facet USING gin (ft_", Name/binary, " public.gin_trgm_ops)">>
    ];
facet_to_index(#facet_def{
        name = Name,
        type = Type
    }) when Type =:= ids;
            Type =:= list ->
    <<"CREATE INDEX search_facet_f_", Name/binary, "_key ",
       "ON search_facet USING gin (f_", Name/binary, ")">>;
facet_to_index(#facet_def{
        name = Name
    }) ->
    <<"CREATE INDEX search_facet_f_", Name/binary, "_key ",
      "ON search_facet(f_", Name/binary, ")">>.


-spec facet_def( Field, Context ) -> {ok, #facet_def{}} | {error, term()}
    when Field :: binary(),
         Context :: z:context().
facet_def(F, Context) ->
    case template_facets(Context) of
        {ok, Defs} ->
            case lists:dropwhile(
                fun(#facet_def{ name = Name }) -> Name =/= F end,
                Defs)
            of
                [ D | _ ] -> {ok, D};
                [] -> {error, enoent}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Fetch all facet definitions from the current facet template.
-spec template_facets( z:context() ) -> {ok, [ facet_def() ]} | {error, term()}.
template_facets(Context) ->
    case z_template:blocks(<<"pivot/facet.tpl">>, #{}, Context) of
        {ok, Blocks} ->
            Facets = lists:filtermap(fun block_to_facet/1, Blocks),
            case find_duplicate_names(Facets) of
                [] -> {ok, Facets};
                Names ->
                    ?LOG_ERROR(#{
                        text => <<"Blocks with duplicate basenames in facet.tpl">>,
                        in => zotonic_mod_search,
                        name => Names,
                        result => error,
                        reason => duplicate_blocks
                    }),
                    {error, duplicate_blocks}
            end;
        {error, _} = Error ->
            Error
    end.

find_duplicate_names(Facets) ->
    Counts = lists:foldl(
        fun(#facet_def{ name = N }, Acc) ->
            Acc#{ N => maps:get(N, Acc, 0) + 1 }
        end,
        #{},
        Facets),
    L = maps:to_list(Counts),
    lists:filtermap(
        fun
            ({_, 1}) -> false;
            ({N, _}) -> {true, N}
        end,
        L).

block_to_facet(Block) ->
    case atom_to_binary(Block, utf8) of
        <<"label_", _/binary>> ->
            % Label blocks are for representation of values
            false;
        <<"is_", _/binary>> = Name ->
            {true, #facet_def{
                name = Name,
                block = Block,
                type = boolean,
                is_range = false
            }};
        B ->
            {Type, Name, IsRange} = block_type(B),
            {true, #facet_def{
                name = Name,
                block = Block,
                type = Type,
                is_range = IsRange
            }}
    end.

block_type(B) ->
    case lists:reverse(binary:split(B, <<"_">>, [ global ])) of

        [ <<"id">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {id, n(Rs), true};
        [ <<"int">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {integer, n(Rs), true};
        [ <<"float">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {float, n(Rs), true};
        [ <<"date">>, <<"range">> | Rs ] when length(Rs) >= 1 ->
            {datetime, n(Rs), true};

        [ <<"id">> | Rs ] when length(Rs) >= 1 ->
            {id, n(Rs), false};
        [ <<"int">> | Rs ] when length(Rs) >= 1 ->
            {integer, n(Rs), false};
        [ <<"float">> | Rs ] when length(Rs) >= 1 ->
            {float, n(Rs), false};
        [ <<"date">> | Rs ] when length(Rs) >= 1 ->
            {datetime, n(Rs), false};

        [ <<"ft">> | Rs ] when length(Rs) >= 1 ->
            {fulltext, n(Rs), false};

        [ <<"list">> | Rs ] when length(Rs) >= 1 ->
            {list, n(Rs), false};

        [ <<"ids">> | Rs ] when length(Rs) >= 1 ->
            {ids, n(Rs), false};

        _ ->
            {text, B, false}
    end.

n(Rs) ->
    iolist_to_binary(lists:join($_, lists:reverse(Rs))).
