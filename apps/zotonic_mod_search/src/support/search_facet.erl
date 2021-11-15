%% @copyright 2021 Driebit BV
%% @doc Faceted search using a facet.tpl for definition and a
%% postgresql table for searches.

%% Copyright 2021 Driebit BV
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
                    | list.
-type facet_def() :: #facet_def{}.


-export([
    facet_values/1,
    search_query_facets/3,
    add_search_arg/4,
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
                    (#facet_def{ type = list }, Acc) ->
                        Acc;
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
                    (#facet_def{ name = Name }, Acc) ->
                        Col = <<"f_", Name/binary>>,
                        Q = <<"select distinct(", Col/binary,") from search_facet ",
                              "where ", Col/binary, " is not null">>,
                        Rs = z_db:q(Q, Context),
                        Rs1 = [ R || {R} <- Rs ],
                        Acc#{
                            Name => #{
                                <<"type">> => <<"value">>,
                                <<"values">> => lists:sort(Rs1)
                            }
                        }
                end,
                #{},
                Facets),
            {ok, FVs};
        {error, _} = Error ->
            Error
    end.



%% @doc Add facets to the result set using the query.
-spec search_query_facets(Result, Query, Context) -> NewResult
    when Result :: #search_result{},
         NewResult :: #search_result{},
         Query :: #search_sql{},
         Context :: z:context().
search_query_facets(Result, Query, Context) ->
    Q1 = join_facet(Query),
    Q2 = Q1#search_sql{
        select = "rsc.id, facet.*",
        limit = undefined
    },
    Q3 = move_unused_order_args_to_select(Q2),
    {SQL, Args} = z_search:concat_sql_query(Q3, undefined),
    SQL2 = [
        "with result as (", SQL, ")"
    ],
    {ok, Defs} = template_facets(Context),
    Unions = lists:map(fun facet_union/1, Defs),
    SQL3 = [
        SQL2,
        " ",
        lists:join("\nunion\n", Unions)
    ],
    FinalSQL = iolist_to_binary(SQL3),
    {ok, Facets} = z_db:qmap(FinalSQL, Args, Context),
    % io:format("~n~s~n", [ FinalSQL ]),
    Fs = group_facets(Defs, Facets),
    NewTotal = facet_total(Fs, Result#search_result.total),
    PageLen = Result#search_result.pagelen,
    Result#search_result{
        facets = Fs,
        total = NewTotal,
        pages = (NewTotal + PageLen - 1) div PageLen
    }.

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
                select = QAcc#search_sql.select
                        ++ ", " ++ z_convert:to_list(Ex) ++ "::character varying"
                }
        end,
        Q,
        InOrder -- InWhere),
    Q1#search_sql{ order = "" }.


facet_total(Fs, Total) ->
    maps:fold(
        fun
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


group_facets(Defs, Facets) ->
    lists:foldl(
        fun
            (#facet_def{ type = list }, Acc) ->
                Acc;
            (#facet_def{ name = Name, is_range = true, type = Type }, Acc) ->
                % value is min, count is max
                [ #{ <<"value">> := Min, <<"count">> := Max } ] = find_facets(Name, Facets),
                Acc#{
                    Name => #{
                        <<"type">> => <<"range">>,
                        <<"min">> => convert_type(Type, Min),
                        <<"max">> => convert_type(Type, Max)
                    }
                };
            (#facet_def{ name = Name, type = Type }, Acc) ->
                Fs = find_facets(Name, Facets),
                Vs = lists:map(
                    fun(#{ <<"value">> := V, <<"count">> := Ct }) ->
                        {binary_to_integer(Ct), V}
                    end,
                    Fs),
                Vs1 = lists:reverse( lists:sort(Vs) ),
                Vs2 = lists:map(
                    fun({Ct, V}) ->
                        #{
                            <<"value">> => convert_type(Type, V),
                            <<"count">> => Ct
                        }
                    end,
                    Vs1),
                Acc#{
                    Name => #{
                        <<"type">> => <<"count">>,
                        <<"counts">> => Vs2
                    }
                }
        end,
        #{},
        Defs).

find_facets(Name, Fs) ->
    lists:filter(
        fun(#{ <<"facet">> := N }) -> Name =:= N end,
        Fs).

facet_union(#facet_def{ type = list }) ->
    [];
facet_union(#facet_def{ name = Name, is_range = true }) ->
    Col = <<"f_", Name/binary>>,
    [
        "select '", Name, "' as facet,
            min(", Col, ")::character varying as value,
            max(", Col, ")::character varying as count
         from result
         where ", Col, " is not null "
    ];
facet_union(#facet_def{ name = Name }) ->
    Col = <<"f_", Name/binary>>,
    [
        "select '", Name, "' as facet,
            ", Col ,"::character varying as value,
            count(*)::character varying as count
        from result
        where ", Col, " is not null
        group by ", Col
    ].


%% @doc Add an extra search argument to the given query. Called by the query
%% builder in search_query.erl
-spec add_search_arg(Field, Value, Query, Context) -> {ok, NewQuery} | {error, term()}
    when Field :: binary(),
         Value :: term(),
         Query :: #search_sql{},
         NewQuery :: #search_sql{},
         Context :: z:context().
add_search_arg(Field, Value, Query, Context) ->
    case facet_def(Field, Context) of
        {ok, Def} ->
            {Op, Value1} = extract_op(Value),
            Value2 = try
                convert_type(Def#facet_def.type, Value1)
            catch _:_ ->
                undefined
            end,
            Query1 = join_facet(Query),
            case Def#facet_def.type of
                fulltext when Op =:= "=" ->
                    NormV = z_string:normalize(Value2),
                    Column = "facet.ft_" ++ binary_to_list(Field),
                    {ArgN, Query2} = add_arg(<<"%", NormV/binary, "%">>, Query1),
                    Query3 = add_where(Column ++ " like " ++ ArgN, Query2);
                _ ->
                    Column = "facet.f_" ++ binary_to_list(Field),
                    {ArgN, Query2} = add_arg(Value2, Query1),
                    Query3 = add_where(Column ++ Op ++ ArgN, Query2)
            end,
            {ok, Query3};
        {error, _} = Error ->
            lager:info("Uknown facet ~p, dropping query term.", [ Field ]),
            Error
    end.

join_facet(#search_sql{ from = From } = Query) ->
    case string:find(From, " search_facet facet") of
        nomatch ->
            From1 = From ++ ", search_facet facet",
            add_where("facet.id = rsc.id", Query#search_sql{ from = From1 });
        _ ->
            Query
    end.

add_where(Clause, Search) ->
    case Search#search_sql.where of
        [] ->
            Search#search_sql{where=Clause};
        C ->
            Search#search_sql{where=C ++ " AND " ++ Clause}
    end.

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

%% Append an argument to a #search_sql
add_arg(ArgValue, Search) ->
    Arg = [$$] ++ integer_to_list(length(Search#search_sql.args) + 1),
    {Arg, Search#search_sql{args=Search#search_sql.args ++ [ArgValue]}}.


%% @doc Pivot all resources to fill the facet table. This runs after every change to the
%% the facet.tpl blocks.
-spec pivot_all( z:context() ) -> ok.
pivot_all(Context) ->
    lager:info("Faceted search: repivoting facet for all resources for ~p", [ z_context:site(Context )]),
    z_pivot_rsc:insert_task_after(1, ?MODULE, pivot_batch, facet_pivot_batch, [0], Context).

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
    {<<"f_", Name/binary>>, convert_type(Type, V)}.


render_block(Block, Template, Vars, Context) ->
    {Output, _RenderState} = z_template:render_block_to_iolist(Block, Template, Vars, Context),
    z_string:trim(iolist_to_binary(Output)).

convert_type(boolean, V) -> z_convert:to_bool(V);
convert_type(_, <<>>) -> undefined;
convert_type(id, V) -> z_convert:to_integer(V);
convert_type(integer, V) -> z_convert:to_integer(V);
convert_type(float, V) -> z_convert:to_float(V);
convert_type(datetime, V) -> z_datetime:to_datetime(V);
convert_type(list, V) ->
    L = binary:split(V, <<",">>, [ global ]),
    L1 = lists:map(fun z_string:trim/1, L),
    lists:filter( fun(B) -> B =/= <<>> end, L1 );
convert_type(fulltext, V) ->
    z_string:truncatechars(z_convert:to_binary(V), ?TEXT_LENGTH);
convert_type(text, V) ->
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
                fun(#column_def{ name = Name, type = Type }) ->
                    is_type(DbCols, Name, Type)
                end,
                TplCols);
        {ok, _} ->
            false;
        {error, _} ->
            false
    end.

is_type([], _Name, _Type) ->
    false;
is_type([ #column_def{ name = Name, type = T } | _ ], Name, Type) ->
    z_convert:to_binary(T) =:= Type;
is_type([ _ | Cols ], Name, Type) ->
    is_type(Cols, Name, Type).


%% @doc Recreate the facet table by first dropping it.
-spec recreate_table( z:context() ) -> ok | {error, term()}.
recreate_table(Context) ->
    lager:info("Faceted search: recreating facet table for ~p", [ z_context:site(Context )]),
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
        type = <<"text">>,
        length = undefined,
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
col_type(float) -> <<"float">>;
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
        type = list
    }) ->
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
            Facets = lists:map(fun block_to_facet/1, Blocks),
            case find_duplicate_names(Facets) of
                [] -> {ok, Facets};
                Names ->
                    lager:error("Blocks with duplicate basenames in facet.tpl: ~p", [ Names ]),
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
        <<"is_", _/binary>> = Name ->
            #facet_def{
                name = Name,
                block = Block,
                type = boolean,
                is_range = false
            };
        B ->
            {Type, Name, IsRange} = block_type(B),
            #facet_def{
                name = Name,
                block = Block,
                type = Type,
                is_range = IsRange
            }
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

        _ ->
            {text, B, false}
    end.

n(Rs) ->
    iolist_to_binary(lists:join($_, lists:reverse(Rs))).
