%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% Date: 2009-04-07
%%
%% @doc Interface to database, uses database definition from Context

%% Copyright 2009-2014 Marc Worrell
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

-module(z_db).
-author("Marc Worrell <marc@worrell.nl>").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-define(TIMEOUT, 30000).

%% interface functions
-export([

    has_connection/1,

    transaction/2,
    transaction/3,
    transaction_clear/1,

    assoc_row/2,
    assoc_row/3,
    assoc_props_row/2,
    assoc_props_row/3,
    assoc/2,
    assoc/3,
    assoc/4,
    assoc_props/2,
    assoc_props/3,
    assoc_props/4,

    qmap_row/2,
    qmap_row/3,
    qmap_row/4,
    qmap_props_row/2,
    qmap_props_row/3,
    qmap_props_row/4,
    qmap/2,
    qmap/3,
    qmap/4,
    qmap_props/2,
    qmap_props/3,
    qmap_props/4,

    q/2,
    q/3,
    q/4,
    q1/2,
    q1/3,
    q1/4,
    q_row/2,
    q_row/3,

    squery/2,
    squery/3,
    equery/2,
    equery/3,
    equery/4,

    insert/2,
    insert/3,
    update/4,
    delete/3,
    select/3,

    column/3,
    columns/2,
    column_names/2,
    column_names_bin/2,
    update_sequence/3,
    prepare_database/1,
    table_exists/2,
    create_table/3,
    drop_table/2,
    flush/1,

    assert_table_name/1,
    prepare_cols/2,

    ensure_database/2,
    ensure_schema/2,
    schema_exists_conn/2,
    drop_schema/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type sql() :: string() | iodata().
-type query_error() :: nodb | enoent | epgsql:query_error() | term().
-type query_timeout() :: integer().

-type transaction_fun() :: fun((z:context()) -> term()).
-type table_name() :: atom() | string().
-type parameters() :: list( parameter() ).
-type parameter() :: tuple()
                   | calendar:datetime()
                   | atom()
                   | string()
                   | binary()
                   | integer()
                   | boolean()
                   | float().
-type props() :: proplists:proplist() | props_map().
-type props_map() :: #{ binary() => term() }.
-type id() :: pos_integer().

-type query_result() :: {ok, Columns :: list(), Rows :: list()}
                      | {ok, Count :: non_neg_integer(), Columns :: list(), Rows :: list()}
                      | {ok, Count :: non_neg_integer()}
                      | {error, term()}.

-export_type([
    sql/0,
    query_error/0,
    query_timeout/0,
    transaction_fun/0,
    parameters/0,
    parameter/0,
    table_name/0,
    props/0,
    id/0
]).


-include_lib("zotonic.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-compile([{parse_transform, lager_transform}]).

%% @doc Perform a function inside a transaction, do a rollback on exceptions
-spec transaction(transaction_fun(), z:context()) -> any() | {error, term()}.
transaction(Function, Context) ->
    transaction(Function, [], Context).

% @doc Perform a transaction with extra options. Default retry on deadlock
-spec transaction(transaction_fun(), list(), z:context()) -> any() | {error, term()}.
transaction(Function, Options, Context) ->
    Result = case transaction1(Function, Context) of
                {rollback, {{error, #error{ codename = deadlock_detected }}, Trace1}} ->
                    {rollback, {deadlock, Trace1}};
                {rollback, {{case_clause, {error, #error{ codename = deadlock_detected }}}, Trace1}} ->
                    {rollback, {deadlock, Trace1}};
                {rollback, {{badmatch, {error, #error{ codename = deadlock_detected }}}, Trace1}} ->
                    {rollback, {deadlock, Trace1}};
                Other ->
                    Other
            end,
    case Result of
        {rollback, {deadlock, Trace}} = DeadlockError ->
            case proplists:get_value(noretry_on_deadlock, Options) of
                true ->
                    lager:warning("DEADLOCK on database transaction, NO RETRY '~p'", [Trace]),
                    DeadlockError;
                _False ->
                    lager:warning("DEADLOCK on database transaction, will retry '~p'", [Trace]),
                    % Sleep random time, then retry transaction
                    timer:sleep(z_ids:number(100)),
                    transaction(Function, Options, Context)
            end;
        R ->
            R
    end.


% @doc Perform the transaction, return error when the transaction function crashed
transaction1(Function, #context{dbc=undefined} = Context) ->
    case has_connection(Context) of
        true ->
            case with_connection(
              fun(C) ->
                    Context1 = Context#context{dbc=C},
                    DbDriver = z_context:db_driver(Context),
                    try
                        case DbDriver:squery(C, "BEGIN", ?TIMEOUT) of
                            {ok, [], []} -> ok;
                            {error, _} = ErrorBegin -> throw(ErrorBegin)
                        end,
                        case Function(Context1) of
                            {rollback, R} ->
                                DbDriver:squery(C, "ROLLBACK", ?TIMEOUT),
                                R;
                            R ->
                                case DbDriver:squery(C, "COMMIT", ?TIMEOUT) of
                                    {ok, [], []} -> ok;
                                    {error, _} = ErrorCommit ->
                                        z_notifier:notify_queue_flush(Context),
                                        throw(ErrorCommit)
                                end,
                               R
                        end
                    catch
                        ?WITH_STACKTRACE(_, Why, S)
                            DbDriver:squery(C, "ROLLBACK", ?TIMEOUT),
                            {rollback, {Why, S}}
                    end
              end,
              Context)
            of
                {rollback, _} = Result ->
                    z_notifier:notify_queue_flush(Context),
                    Result;
                Result ->
                    z_notifier:notify_queue(Context),
                    Result
            end;
        false ->
            {rollback, {no_database_connection, []}}
    end;
transaction1(Function, Context) ->
    % Nested transaction, only keep the outermost transaction
    Function(Context).


%% @doc Clear any transaction in the context, useful when starting a thread with this context.
transaction_clear(#context{dbc=undefined} = Context) ->
    Context;
transaction_clear(Context) ->
    Context#context{dbc=undefined}.


%% @doc Check if we have database connection up and runnng
-spec has_connection(z:context() | atom()) -> boolean().
has_connection(Site) when is_atom(Site) ->
    is_pid(erlang:whereis(z_db_pool:db_pool_name(Site)));
has_connection(Context) ->
    is_pid(erlang:whereis(z_context:db_pool(Context))).


%% @doc Transaction handler safe function for fetching a db connection
get_connection(#context{dbc=undefined} = Context) ->
    case has_connection(Context) of
        true ->
            set_dbtrace_flag(Context),
            z_db_pool:get_connection(Context);
        false ->
            none
    end;
get_connection(Context) ->
    Context#context.dbc.

set_dbtrace_flag(Context) ->
    case erlang:get(is_dbtrace) of
        true -> ok;
        false -> ok;
        _ ->
            IsTrace = case z_notifier:first({server_storage, secure_lookup, is_dbtrace, undefined}, Context) of
                {ok, true} -> true;
                _ -> false
            end,
            erlang:put(is_dbtrace, IsTrace),
            ok
    end.

%% @doc Transaction handler safe function for releasing a db connection
return_connection(C, Context=#context{dbc=undefined}) ->
    z_db_pool:return_connection(C, Context);
return_connection(_C, _Context) ->
    ok.

%% @doc Apply function F with a connection as parameter. Make sure the
%% connection is returned after usage.
-spec with_connection(fun(), z:context()) -> any().
with_connection(F, Context) ->
    with_connection(F, get_connection(Context), Context).

with_connection(F, none, _Context) ->
    F(none);
with_connection(F, Connection, Context) when is_pid(Connection) ->
    exometer:update([zotonic, z_context:site(Context), db, requests], 1),
    try
        {Time, Result} = timer:tc(F, [Connection]),
        exometer:update([zotonic, z_context:site(Context), db, duration], Time),
        Result
    after
        return_connection(Connection, Context)
end.


%% ----------------------------------------------------------------
%% Query - return proplists
%% ----------------------------------------------------------------

-spec assoc_row(string(), z:context()) -> proplists:proplist().
assoc_row(Sql, Context) ->
    assoc_row(Sql, [], Context).

-spec assoc_row(string(), parameters(), z:context()) -> proplists:proplist() | undefined.
assoc_row(Sql, Parameters, Context) ->
    case assoc(Sql, Parameters, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.

-spec assoc_props_row(string(), z:context()) -> proplists:proplist() | undefined.
assoc_props_row(Sql, Context) ->
    assoc_props_row(Sql, [], Context).

-spec assoc_props_row(string(), list(), z:context()) -> proplists:proplist() | undefined.
assoc_props_row(Sql, Parameters, Context) ->
    case assoc_props(Sql, Parameters, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.

%% @doc Return property lists of the results of a query on the database in the Context
-spec assoc(sql(), z:context()) -> list( proplists:proplist() ).
assoc(Sql, Context) ->
    assoc(Sql, [], Context).

-spec assoc(sql(), list(), z:context()) -> list( proplists:proplist() ).
assoc(Sql, Parameters, #context{} = Context) ->
    assoc(Sql, Parameters, Context, ?TIMEOUT);
assoc(Sql, #context{} = Context, Timeout) when is_integer(Timeout) ->
    assoc(Sql, [], Context, Timeout).

-spec assoc(sql(), list(), z:context(), integer()) -> list( proplists:proplist() ).
assoc(Sql, Parameters, Context, Timeout) ->
    DbDriver = z_context:db_driver(Context),
    F = fun
       (none) -> [];
       (C) ->
            case assoc1(DbDriver, C, Sql, Parameters, Timeout) of
                {ok, Result} when is_list(Result) ->
                    Result
            end
    end,
    with_connection(F, Context).


-spec assoc_props(sql(), z:context()) -> list( proplists:proplist() ).
assoc_props(Sql, Context) ->
    assoc_props(Sql, [], Context).

-spec assoc_props(sql(), list(), z:context()) -> list( proplists:proplist() ).
assoc_props(Sql, Parameters, #context{} = Context) ->
    assoc_props(Sql, Parameters, Context, ?TIMEOUT);
assoc_props(Sql, #context{} = Context, Timeout) when is_integer(Timeout) ->
    assoc_props(Sql, [], Context, Timeout).

-spec assoc_props(sql(), list(), z:context(), integer()) -> list( proplists:proplist() ).
assoc_props(Sql, Parameters, Context, Timeout) ->
    DbDriver = z_context:db_driver(Context),
    F = fun
        (none) -> [];
        (C) ->
            case assoc1(DbDriver, C, Sql, Parameters, Timeout) of
                {ok, Result} when is_list(Result) ->
                    merge_props(Result)
            end
    end,
    with_connection(F, Context).


%% ----------------------------------------------------------------
%% Query - return maps and error codes
%% ----------------------------------------------------------------

-spec qmap_row( sql(), z:context() ) -> {ok, map()} | {error, query_error()}.
qmap_row(Sql, Context) ->
    qmap_row(Sql, [], [], Context).

-spec qmap_row( sql(), parameters(), z:context() ) -> {ok, map()} | {error, query_error()}.
qmap_row(Sql, Args, Context) ->
    qmap_row(Sql, Args, [], Context).

-spec qmap_row( sql(), parameters(), list(), z:context()) -> {ok, map()} | {error, query_error()}.
qmap_row(Sql, Args, Options, Context) ->
    case qmap(Sql, Args, Options, Context) of
        {ok, [ M | _ ]} when is_map(M) ->
            {ok, M};
        {ok, []} ->
            {error, enoent};
        Other ->
            Other
    end.

-spec qmap_props_row( sql(), z:context() ) -> {ok, map()} | {error, query_error()}.
qmap_props_row(Sql, Context) ->
    qmap_props_row(Sql, [], [], Context).

-spec qmap_props_row( sql(), parameters(), z:context() ) -> {ok, map()} | {error, query_error()}.
qmap_props_row(Sql, Args, Context) ->
    qmap_props_row(Sql, Args, [], Context).

-spec qmap_props_row( sql(), parameters(), list(), z:context()) -> {ok, map()} | {error, query_error()}.
qmap_props_row(Sql, Args, Options, Context) ->
    case qmap_props(Sql, Args, Options, Context) of
        {ok, [ M | _ ]} when is_map(M) ->
            {ok, M};
        {ok, []} ->
            {error, enoent};
        Other ->
            Other
    end.

-spec qmap( sql(), z:context() ) -> {ok, [ map() ]} | {error, query_error()}.
qmap(Sql, Context) ->
    qmap(Sql, [], [], Context).

-spec qmap( sql(), parameters(), z:context() ) -> {ok, [ map() ]} | {error, query_error()}.
qmap(Sql, Args, Context) ->
    qmap(Sql, Args, [], Context).

-spec qmap( sql(), parameters(), list(), z:context() ) -> {ok, [ map() ]} | {error, query_error()}.
qmap(Sql, Args, Options, Context) ->
    DbDriver = z_context:db_driver(Context),
    F = fun
        (none) ->
            {error, nodb};
        (C) ->
            Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
            Keys = proplists:get_value(keys, Options, binary),
            case DbDriver:equery(C, Sql, Args, Timeout) of
                {ok, _Affected, Cols, Rows} when is_list(Rows) ->
                    {ok, cols_map(Cols, Rows, false, Keys)};
                {ok, Cols, Rows} when is_list(Rows) ->
                    {ok, cols_map(Cols, Rows, false, Keys)};
                {ok, Value} when is_list(Value); is_integer(Value) ->
                    {ok, Value};
                {error, Reason} = Error ->
                    lager:error("z_db error ~p in query ~s with ~p", [Reason, Sql, Args]),
                    Error
            end

    end,
    with_connection(F, Context).

-spec qmap_props( sql(), z:context() ) -> {ok, [ map() ]} | {error, query_error()}.
qmap_props(Sql, Context) ->
    qmap_props(Sql, [], [], Context).

-spec qmap_props( sql(), parameters(), z:context() ) -> {ok, [ map() ]} | {error, query_error()}.
qmap_props(Sql, Args, Context) ->
    qmap_props(Sql, Args, [], Context).

-spec qmap_props( sql(), parameters(), list(), z:context() ) -> {ok, [ map() ]} | {error, query_error()}.
qmap_props(Sql, Args, Options, Context) ->
    DbDriver = z_context:db_driver(Context),
    F = fun
        (none) ->
            {error, nodb};
        (C) ->
            Timeout = proplists:get_value(timeout, Options, ?TIMEOUT),
            Keys = proplists:get_value(keys, Options, binary),
            case DbDriver:equery(C, Sql, Args, Timeout) of
                {ok, _Affected, Cols, Rows} when is_list(Rows) ->
                    {ok, cols_map(Cols, Rows, true, Keys)};
                {ok, Cols, Rows} when is_list(Rows) ->
                    {ok, cols_map(Cols, Rows, true, Keys)};
                {ok, Value} when is_list(Value); is_integer(Value) ->
                    {ok, Value};
                {error, Reason} = Error ->
                    lager:error("z_db error ~p in query ~s with ~p", [Reason, Sql, Args]),
                    Error
            end

    end,
    with_connection(F, Context).


%% @doc Make associative maps from all the rows in the result set.
cols_map(Cols, Rows, IsMergeProps, Keys) ->
    ColNames = [ Name || #column{ name = Name } <- Cols ],
    ColNames1 = case Keys of
        atom -> [ binary_to_atom(K, utf8) || K <- ColNames ];
        binary -> ColNames
    end,
    ColIndices = lists:zip( lists:seq(1, length(ColNames1)), ColNames1 ),
    lists:map(
        fun(Row) ->
            lists:foldl(
                fun
                    ({Nr, <<"props">>}, Acc) when IsMergeProps ->
                        Props = erlang:element(Nr, Row),
                        map_merge_props(Props, Acc);
                    ({Nr, props}, Acc) when IsMergeProps ->
                        Props = erlang:element(Nr, Row),
                        map_merge_props(Props, Acc);
                    ({Nr, Col}, Acc) ->
                        Acc#{ Col => erlang:element(Nr, Row) }
                end,
                #{},
                ColIndices)
        end,
        Rows).

map_merge_props(M, Acc) when is_map(M) ->
    maps:merge(Acc, M);
map_merge_props(Props, Acc) when is_list(Props) ->
    maps:merge(Acc, z_props:from_props(Props));
map_merge_props(_, Acc) ->
    Acc.


%% ----------------------------------------------------------------
%% Simple queries - return tuples or number of rows updated.
%% ----------------------------------------------------------------

q(Sql, Context) ->
    q(Sql, [], Context, ?TIMEOUT).

q(Sql, Parameters, #context{} = Context) ->
    q(Sql, Parameters, Context, ?TIMEOUT);
q(Sql, #context{} = Context, Timeout) when is_integer(Timeout) ->
    q(Sql, [], Context, Timeout).

q(Sql, Parameters, Context, Timeout) ->
    F = fun
        (none) -> [];
        (C) ->
            DbDriver = z_context:db_driver(Context),
            case DbDriver:equery(C, Sql, Parameters, Timeout) of
                {ok, _Affected, _Cols, Rows} when is_list(Rows) -> Rows;
                {ok, _Cols, Rows} when is_list(Rows) -> Rows;
                {ok, Value} when is_list(Value); is_integer(Value) -> Value;
                {error, Reason} = Error ->
                    lager:error("z_db error ~p in query ~s with ~p", [Reason, Sql, Parameters]),
                    throw(Error)
            end
    end,
    with_connection(F, Context).

q1(Sql, Context) ->
    q1(Sql, [], Context).

q1(Sql, Parameters, #context{} = Context) ->
    q1(Sql, Parameters, Context, ?TIMEOUT);
q1(Sql, #context{} = Context, Timeout) when is_integer(Timeout) ->
    q1(Sql, [], Context, Timeout).

-spec q1(sql(), parameters(), #context{}, pos_integer()) -> term() | undefined.
q1(Sql, Parameters, Context, Timeout) ->
    F = fun
        (none) -> undefined;
        (C) ->
            DbDriver = z_context:db_driver(Context),
            case equery1(DbDriver, C, Sql, Parameters, Timeout) of
                {ok, Value} -> Value;
                {error, noresult} -> undefined;
                {error, Reason} = Error ->
                    lager:error("z_db error ~p in query ~s with ~p", [Reason, Sql, Parameters]),
                    throw(Error)
            end
    end,
    with_connection(F, Context).


q_row(Sql, Context) ->
    q_row(Sql, [], Context).

q_row(Sql, Args, Context) ->
    case q(Sql, Args, Context) of
        [Row|_] -> Row;
        [] -> undefined
    end.


squery(Sql, Context) ->
    squery(Sql, Context, ?TIMEOUT).

squery(Sql, Context, Timeout) when is_integer(Timeout) ->
    F = fun(C) when C =:= none -> {error, noresult};
           (C) ->
                DbDriver = z_context:db_driver(Context),
                DbDriver:squery(C, Sql, Timeout)
        end,
    with_connection(F, Context).


equery(Sql, Context) ->
    equery(Sql, [], Context).

equery(Sql, Parameters, #context{} = Context) ->
    equery(Sql, Parameters, Context, ?TIMEOUT);
equery(Sql, #context{} = Context, Timeout) when is_integer(Timeout) ->
    equery(Sql, [], Context, Timeout).

-spec equery(sql(), parameters(), z:context(), integer()) -> query_result().
equery(Sql, Parameters, Context, Timeout) ->
    F = fun(C) when C =:= none -> {error, noresult};
           (C) ->
                DbDriver = z_context:db_driver(Context),
                DbDriver:equery(C, Sql, Parameters, Timeout)
        end,
    with_connection(F, Context).


%% @doc Insert a new row in a table, use only default values.
-spec insert(table_name(), z:context()) -> {ok, pos_integer()|undefined} | {error, term()}.
insert(Table, Context) when is_atom(Table) ->
    insert(atom_to_list(Table), Context);
insert(Table, Context) ->
    assert_table_name(Table),
    with_connection(
      fun(C) ->
              DbDriver = z_context:db_driver(Context),
              equery1(DbDriver, C, "insert into \""++Table++"\" default values returning id")
      end,
      Context).


%% @doc Insert a row, setting the fields to the props. Unknown columns are
%% serialized in the props column. When the table has an 'id' column then the
%% new id is returned.
-spec insert(table_name(), props(), z:context()) -> {ok, integer()|undefined} | {error, term()}.
insert(Table, Parameters, Context) when is_list(Parameters) ->
    insert(Table, z_props:from_props(Parameters), Context);
insert(Table, Parameters, Context) when is_atom(Table) ->
    insert(atom_to_list(Table), Parameters, Context);
insert(Table, Parameters, Context) ->
    assert_table_name(Table),
    Cols = column_names_bin(Table, Context),
    BinParams = ensure_binary_keys(Parameters),
    case prepare_cols(Cols, BinParams) of
        {ok, InsertProps} ->
            HasProps = maps:is_key(<<"props">>, InsertProps), 
            HasPropsJSON = maps:is_key(<<"props_json">>, InsertProps), 

            InsertProps1 = if HasPropsJSON ->
                                   #{<<"props_json">> := PropsJSONCol} = InsertProps,
                                   case is_map(PropsJSONCol) of
                                       true ->
                                           InsertProps#{
                                             <<"props_json">> => ?DB_PROPS_JSON(filter_empty_props(PropsJSONCol))
                                            };
                                       false ->
                                           InsertProps
                                   end;
                               HasProps ->
                                   #{<<"props">> := PropsCol} = InsertProps,
                                   case is_map(PropsCol) of
                                       true ->
                                           InsertProps#{
                                             <<"props">> => ?DB_PROPS(filter_empty_props(PropsCol))
                                            };
                                       false ->
                                           InsertProps
                                   end;
                               not HasPropsJSON and not HasProps ->
                                   InsertProps
                           end,
            
            %% Build the SQL insert statement
            {ColNames, ColParams} = lists:unzip( maps:to_list(InsertProps1) ),
            Sql = iolist_to_binary([
                "insert into \"", Table, "\" (\"",
                    lists:join("\", \"", ColNames),
                "\") values (",
                    lists:join(", ", [ [$$ | integer_to_list(N)] || N <- lists:seq(1, length(ColParams)) ]),
                ")"
            ]),
            FinalSql = case lists:member(<<"id">>, Cols) of
                true -> <<Sql/binary, " returning id">>;
                false -> Sql
            end,

            F = fun(C) ->
                 DbDriver = z_context:db_driver(Context),
                 case equery1(DbDriver, C, FinalSql, ColParams) of
                     {ok, Id} ->
                        {ok, Id};
                     {error, noresult} ->
                        {ok, undefined};
                     {error, #error{ codename = unique_violation }} = Error ->
                        lager:info("z_db unique_violation in insert to ~p of ~p", [Table, Parameters]),
                        Error;
                     {error, Reason} = Error ->
                        lager:error("z_db error ~p in query \"~s\" with ~p", [Reason, FinalSql, ColParams]),
                        Error
                 end
            end,
            with_connection(F, Context);
        {error, _} = Error ->
            Error
    end.


%% @doc Update a row in a table, merging the props list with any new props values
-spec update(table_name(), id(), props(), z:context()) -> {ok, RowsUpdated::integer()} | {error, term()}.
update(Table, Id, Parameters, Context) when is_list(Parameters) ->
    update(Table, Id, z_props:from_props(Parameters), Context);
update(Table, Id, Parameters, Context) when is_atom(Table) ->
    update(atom_to_list(Table), Id, Parameters, Context);
update(Table, Id, Parameters, Context) when is_map(Parameters), is_list(Table) ->
    assert_table_name(Table),
    DbDriver = z_context:db_driver(Context),
    Cols = column_names_bin(Table, Context),
    BinParams = ensure_binary_keys(Parameters),
    case prepare_cols(Cols, BinParams) of
        {ok, UpdateProps} ->
            F = fun(C) ->
                UpdateProps1 = case maps:get(<<"props">>, UpdateProps, undefined) of
                    NewProps when is_map(NewProps) ->
                        % Merge the new props with the props in the database
                        case equery1(DbDriver, C, "select props from \""++Table++"\" where id = $1", [Id]) of
                            {ok, OldProps} when is_list(OldProps) ->
                                NewProps1 = maps:merge(z_props:from_props(OldProps), NewProps),
                                UpdateProps#{ <<"props">> => ?DB_PROPS( filter_empty_props(NewProps1) ) };
                            {ok, OldProps} when is_map(OldProps) ->
                                NewProps1 = maps:merge(OldProps, NewProps),
                                UpdateProps#{ <<"props">> => ?DB_PROPS( filter_empty_props(NewProps1) ) };
                            _ ->
                                UpdateProps#{ <<"props">> => ?DB_PROPS( filter_empty_props(NewProps) ) }
                        end;
                    _ ->
                        UpdateProps
                end,
                {ColNames,Params} = lists:unzip( maps:to_list(UpdateProps1) ),
                ColNamesNr = lists:zip(ColNames, lists:seq(2, length(ColNames)+1)),
                ColAssigns = [
                    ["\"", ColName, "\" = $", integer_to_list(Nr)]
                    || {ColName, Nr} <- ColNamesNr
                ],
                Sql = iolist_to_binary([
                    "update \"", Table, "\" set ",
                    lists:join(", ", ColAssigns),
                    " where id = $1"
                ]),
                case equery1(DbDriver, C, Sql, [Id | Params]) of
                    {ok, _RowsUpdated} = Ok -> Ok;
                    {error, Reason} = Error ->
                        lager:error("z_db error ~p in query ~s with ~p", [Reason, Sql, [Id | Params]]),
                        Error
                end
            end,
            with_connection(F, Context);
        {error, _} = Error ->
            Error
    end.

ensure_binary_keys(Ps) ->
    maps:fold(
        fun
            (K, V, Acc) when is_atom(K) ->
                Acc#{
                    atom_to_binary(K, utf8) => V
                };
            (K, V, Acc) when is_binary(K) ->
                Acc#{
                    K => V
                }
        end,
        #{},
        Ps).

filter_empty_props(Map) ->
    maps:filter(
        fun
            (_K, undefined) -> false;
            (_K, <<>>) -> false;
            (_K, #trans{ tr = Tr }) ->
                lists:any(
                    fun
                        ({_, <<>>}) -> false;
                        (_) -> true
                    end,
                    Tr);
            (_K, _V) ->
                true
        end,
        Map).


%% @doc Delete a row from a table, the row must have a column with the name 'id'
-spec delete(Table::table_name(), Id::integer(), z:context()) -> {ok, RowsDeleted::non_neg_integer()} | {error, term()}.
delete(Table, Id, Context) when is_atom(Table) ->
    delete(atom_to_list(Table), Id, Context);
delete(Table, Id, Context) ->
    assert_table_name(Table),
    Sql = "delete from \""++Table++"\" where id = $1",
    case equery(Sql, [Id], Context) of
        {ok, _} = Ok ->
            Ok;
        {error, _} = Error ->
            Error
    end.


%% @doc Read a row from a table, the row must have a column with the name 'id'.
%% The props column contents is merged with the other properties returned.
-spec select(table_name(), any(), z:context()) -> {ok, Row :: map()} | {error, term()}.
select(Table, Id, Context) ->
    select(Table, Id, [], Context).


-spec select(table_name(), any(), list(), z:context()) -> {ok, Row :: map()} | {error, term()}.
select(Table, Id, Options, Context) when is_atom(Table) ->
    select(atom_to_list(Table), Id, Options, Context);
select(Table, Id, Options, Context) ->
    assert_table_name(Table),
    Sql = "select * from \""++Table++"\" where id = $1 limit 1",
    qmap_props_row(Sql, [ Id ], Options, Context).


%% @doc Check if all cols are valid columns in the target table, move unknown properties
%%      to the props column (if exists).
prepare_cols(Cols, Props) ->
    case split_props(Props, Cols) of
        {ok, {CProps, PProps}} ->
            case maps:size(PProps) of
                0 ->
                    {ok, CProps};
                _  ->
                    case lists:member(<<"props_json">>, Cols) of
                        true ->
                            PropsCol = merge_properties(maps:get(<<"props_json">>, CProps, undefined), PProps),
                            {ok, CProps#{ <<"props_json">> => PropsCol }};
                        false ->
                            PropsCol = merge_properties(maps:get(<<"props">>, CProps, undefined), PProps),
                            {ok, CProps#{ <<"props">> => PropsCol }}
                    end
            end;
        {error, _} = Error ->
            Error
    end.

merge_properties(Properties, Props) when is_list(Properties) ->
    maps:merge(z_props:from_props(Properties), Props);
merge_properties(Properties, Props) when is_map(Properties) ->
    maps:merge(Properties, Props);
merge_properties(_, Props) ->
    Props.

split_props(Props, Cols) ->
    {CProps, PProps} = lists:foldl(
        fun(Col, {CAcc, PAcc}) ->
            case maps:find(Col, PAcc) of
                {ok, V} ->
                    PAcc1 = maps:remove(Col, PAcc),
                    CAcc1 = CAcc#{ Col => V },
                    {CAcc1, PAcc1};
                error ->
                    {CAcc, PAcc}
            end
        end,
        {#{}, Props},
        Cols),
    case maps:size(PProps) of
        0 ->
            {ok, {CProps, PProps}};
        _  ->
            case lists:member(<<"props_json">>, Cols) orelse lists:member(<<"props">>, Cols) of
                true ->
                    {ok, {CProps, PProps}};
                false ->
                    Ks = maps:keys(PProps),
                    {error, {unknown_column, Ks}}
            end
    end.


%% @doc Return a property list with all columns of the table. (example: [{id,int4,modifier},...])
-spec columns(atom()|string(), z:context()) -> list( #column_def{} ).
columns(Table, Context) when is_atom(Table) ->
    columns(atom_to_list(Table), Context);
columns(Table, Context) ->
    Options = z_db_pool:get_database_options(Context),
    Db = proplists:get_value(dbdatabase, Options),
    Schema = proplists:get_value(dbschema, Options),
    case z_depcache:get({columns, Db, Schema, Table}, Context) of
        {ok, Cols} ->
            Cols;
        _ ->
            Cols = q("  select column_name, data_type, character_maximum_length, is_nullable, column_default
                        from information_schema.columns
                        where table_catalog = $1
                          and table_schema = $2
                          and table_name = $3
                        order by ordinal_position", [Db, Schema, Table], Context),
            Cols1 = [ columns1(Col) || Col <- Cols ],
            z_depcache:set({columns, Db, Schema, Table}, Cols1, ?YEAR, [{database, Db}], Context),
            Cols1
    end.


columns1({<<"id">>, <<"integer">>, undefined, Nullable, <<"nextval(", _/binary>>}) ->
    #column_def{
        name = id,
        type = "serial",
        length = undefined,
        is_nullable = z_convert:to_bool(Nullable),
        default = undefined
    };
columns1({Name,Type,MaxLength,Nullable,Default}) ->
    #column_def{
        name = z_convert:to_atom(Name),
        type = z_convert:to_list(Type),
        length = MaxLength,
        is_nullable = z_convert:to_bool(Nullable),
        default = column_default(Default)
    }.

column_default(undefined) -> undefined;
column_default(<<"nextval(", _/binary>>) -> undefined;
column_default(Default) -> binary_to_list(Default).


-spec column( atom() | string(), atom() | string(), z:context()) ->
        {ok, #column_def{}} | {error, enoent}.
column(Table, Column0, Context) ->
    Column = z_convert:to_atom(Column0),
    Columns = columns(Table, Context),
    case lists:filter(
        fun
            (#column_def{ name = Name }) when Name =:= Column -> true;
            (_) -> false
        end,
        Columns)
    of
        [] -> {error, enoent};
        [#column_def{} = Col] -> {ok, Col}
    end.

%% @doc Return a list with the column names of a table.  The names are sorted.
-spec column_names(table_name(), z:context()) -> list( atom() ).
column_names(Table, Context) ->
    Names = [ C#column_def.name || C <- columns(Table, Context)],
    lists:sort(Names).

-spec column_names_bin(table_name(), z:context()) -> list( binary() ).
column_names_bin(Table, Context) ->
    [ atom_to_binary(Col, utf8) || Col <- column_names(Table, Context) ].


%% @doc Flush all cached information about the database.
flush(Context) ->
    Options = z_db_pool:get_database_options(Context),
    Db = proplists:get_value(dbdatabase, Options),
    z_depcache:flush({database, Db}, Context).


%% @doc Update the sequence of the ids in the table. They will be renumbered according to their position in the id list.
%% @todo Make the steps of the sequence bigger, and try to keep the old sequence numbers in tact (needs a diff routine)
-spec update_sequence(table_name(), list( integer() ), z:context()) -> any().
update_sequence(Table, Ids, Context) when is_atom(Table) ->
    update_sequence(atom_to_list(Table), Ids, Context);
update_sequence(Table, Ids, Context) ->
    assert_table_name(Table),
    DbDriver = z_context:db_driver(Context),
    Args = lists:zip(Ids, lists:seq(1, length(Ids))),
    F = fun
        (none) ->
            [];
        (C) ->
            [ {ok, _} = equery1(DbDriver, C, "update \""++Table++"\" set seq = $2 where id = $1", tuple_to_list(Arg)) || Arg <- Args ]
    end,
    with_connection(F, Context).

%% @doc Create database and schema if they do not yet exist
-spec prepare_database(#context{}) -> ok | {error, term()}.
prepare_database(Context) ->
    Options = z_db_pool:get_database_options(Context),
    Site = z_context:site(Context),
    case ensure_database(Site, Options) of
        ok ->
            ensure_schema(Site, Options);
        {error, _} = Error ->
            Error
    end.

ensure_database(Site, Options) ->
    Database = proplists:get_value(dbdatabase, Options),
    case open_connection("postgres", Options) of
        {ok, PgConnection} ->
            Result = case database_exists(PgConnection, Database) of
                true ->
                    ok;
                false ->
                    AnonOptions = proplists:delete(dbpassword, Options),
                    lager:warning("Creating database ~p with options: ~p", [Database, AnonOptions]),
                    create_database(Site, PgConnection, Database)
            end,
            close_connection(PgConnection),
            Result;
        {error, Reason} = Error ->
            lager:error("Cannot create database ~p because user ~p cannot connect to the 'postgres' database: ~p",
                        [Database, proplists:get_value(dbuser, Options), Reason]),
            Error
    end.

ensure_schema(Site, Options) ->
    Database = proplists:get_value(dbdatabase, Options),
    Schema = proplists:get_value(dbschema, Options),
    {ok, DbConnection} = open_connection(Database, Options),
    Result = case schema_exists_conn(DbConnection, Schema) of
        true ->
            ok;
        false ->
            lager:info("Creating schema ~p in database ~p", [Schema, Database]),
            create_schema(Site, DbConnection, Schema)
    end,
    close_connection(DbConnection),
    Result.

drop_schema(Context) ->
    Options = z_db_pool:get_database_options(Context),
    Schema = proplists:get_value(dbschema, Options),
    Database = proplists:get_value(dbdatabase, Options),
    case open_connection(Database, Options) of
        {ok, DbConnection} ->
            Result = case schema_exists_conn(DbConnection, Schema) of
                true ->
                    case epgsql:equery(
                        DbConnection,
                        "DROP SCHEMA \"" ++ Schema ++ "\" CASCADE"
                    ) of
                        {ok, _, _} = OK ->
                            lager:warning("Dropped schema ~p (~p)", [Schema, OK]),
                            ok;
                        {error, Reason} = Error ->
                            lager:error("z_db error ~p when dropping schema ~p", [Reason, Schema]),
                            Error
                    end;
                false ->
                    lager:warning("Could not drop schema ~p as it does not exist", [Schema]),
                    ok
            end,
            close_connection(DbConnection),
            Result;
        {error, Reason} ->
            lager:error("z_db error ~p when connecting for dropping schema ~p", [Reason, Schema]),
            ok
    end.

open_connection(DatabaseName, Options) ->
    epgsql:connect(
        proplists:get_value(dbhost, Options),
        proplists:get_value(dbuser, Options),
        proplists:get_value(dbpassword, Options),
        [
            {port, proplists:get_value(dbport, Options)},
            {database, DatabaseName}
        ]
    ).

close_connection(Connection) ->
    epgsql:close(Connection).

%% @doc Check whether database exists
-spec database_exists(epgsql:connection(), string()) -> boolean().
database_exists(Connection, Database) ->
    {ok, _, [{Count}]} = epgsql:equery(
        Connection,
        "SELECT COUNT(*) FROM pg_catalog.pg_database WHERE datname = $1",
        [Database]
    ),
    case z_convert:to_integer(Count) of
        0 -> false;
        1 -> true
    end.

%% @doc Create a database
-spec create_database(atom(), epgsql:connection(), string()) -> ok | {error, term()}.
create_database(_Site, Connection, Database) ->
    %% Use template0 to prevent ERROR: new encoding (UTF8) is incompatible with
    %% the encoding of the template database (SQL_ASCII)
    case epgsql:equery(
        Connection,
        "CREATE DATABASE \"" ++ Database ++ "\" ENCODING = 'UTF8' TEMPLATE template0"
    ) of
        {error, Reason} = Error ->
            lager:error("z_db error ~p when creating database ~p", [Reason, Database]),
            Error;
        {ok, _, _} ->
            ok
    end.

%% @doc Check whether schema exists
-spec schema_exists_conn(epgsql:connection(), string()) -> boolean().
schema_exists_conn(Connection, Schema) ->
    {ok, _, Schemas} = epgsql:equery(
        Connection,
        "SELECT schema_name FROM information_schema.schemata"),
    lists:member( {z_convert:to_binary(Schema)}, Schemas ).

%% @doc Create a schema
-spec create_schema(atom(), epgsql:connection(), string()) -> ok | {error, term()}.
create_schema(_Site, Connection, Schema) ->
    case epgsql:equery(
        Connection,
        "CREATE SCHEMA \"" ++ Schema ++ "\""
    ) of
        {ok, _, _} ->
            ok;
        {error, #error{ codename = duplicate_schema, message = Msg }} ->
            lager:warning("Schema already exists ~p (~p)", [Schema, Msg]),
            ok;
        {error, Reason} = Error ->
            lager:error("z_db error ~p when creating schema ~p", [Reason, Schema]),
            Error
    end.

%% @doc Check the information schema if a certain table exists in the context database.
-spec table_exists(table_name(), z:context()) -> boolean().
table_exists(Table, Context) ->
    Options = z_db_pool:get_database_options(Context),
    Db = proplists:get_value(dbdatabase, Options),
    Schema = proplists:get_value(dbschema, Options),
    case q1("   select count(*)
                from information_schema.tables
                where table_catalog = $1
                  and table_name = $2
                  and table_schema = $3
                  and table_type = 'BASE TABLE'", [Db, Table, Schema], Context) of
        1 -> true;
        0 -> false
    end.


%% @doc Make sure that a table is dropped, only when the table exists
-spec drop_table(table_name(), z:context()) -> ok.
drop_table(Name, Context) when is_atom(Name) ->
    drop_table(atom_to_list(Name), Context);
drop_table(Name, Context) ->
    case table_exists(Name, Context) of
        true -> q("drop table \"" ++ Name ++ "\"", Context), ok;
        false -> ok
    end.


%% @doc Ensure that a table with the given columns exists, alter any existing table
%% to add, modify or drop columns.  The 'id' (with type serial) column _must_ be defined
%% when creating the table.
-spec create_table(table_name(), list(), z:context()) -> ok.
create_table(Table, Cols, Context) when is_atom(Table)->
    create_table(atom_to_list(Table), Cols, Context);
create_table(Table, Cols, Context) ->
    assert_table_name(Table),
    ColsSQL = ensure_table_create_cols(Cols, []),
    z_db:q("CREATE TABLE \""++Table++"\" ("++string:join(ColsSQL, ",")
        ++ table_create_primary_key(Cols) ++ ")", Context),
    ok.


table_create_primary_key([]) -> [];
table_create_primary_key([#column_def{name=id, type="serial"}|_]) -> ", primary key(id)";
table_create_primary_key([#column_def{name=N, primary_key=true}|_]) -> ", primary key(" ++ z_convert:to_list(N) ++ ")";
table_create_primary_key([_|Cols]) -> table_create_primary_key(Cols).

ensure_table_create_cols([], Acc) ->
    lists:reverse(Acc);
ensure_table_create_cols([C|Cols], Acc) ->
    M = lists:flatten([$", atom_to_list(C#column_def.name), $", 32, column_spec(C)]),
    ensure_table_create_cols(Cols, [M|Acc]).

column_spec(#column_def{type=Type, length=Length, is_nullable=Nullable, default=Default, unique=Unique}) ->
    L = case Length of
            undefined -> [];
            _ -> [$(, integer_to_list(Length), $)]
        end,
    N = column_spec_nullable(Nullable),
    D = column_spec_default(Default),
    U = column_spec_unique(Unique),
    lists:flatten([Type, L, N, D, U]).

column_spec_nullable(true) -> "";
column_spec_nullable(false) -> " not null".

column_spec_default(undefined) -> "";
column_spec_default(Default) -> [" DEFAULT ", Default].

column_spec_unique(false) -> "";
column_spec_unique(true) -> " UNIQUE".

%% @doc Check if a name is a valid SQL table name. Crashes when invalid
-spec assert_table_name(string()) -> true.
assert_table_name([H|T]) when (H >= $a andalso H =< $z) orelse H == $_ ->
    assert_table_name1(T).
assert_table_name1([]) ->
    true;
assert_table_name1([H|T]) when (H >= $a andalso H =< $z) orelse (H >= $0 andalso H =< $9) orelse H == $_ ->
    assert_table_name1(T).



%% @doc Merge the contents of the props column into the result rows
-spec merge_props([proplists:proplist() | map()]) -> list() | undefined.
merge_props(List) ->
    merge_props(List, []).

merge_props([], Acc) ->
    lists:reverse(Acc);
merge_props([R|Rest], Acc) when is_list(R) ->
    case proplists:get_value(props, R, undefined) of
        undefined ->
            merge_props(Rest, [R|Acc]);
        <<>> ->
            merge_props(Rest, [R|Acc]);
        Term when is_list(Term) ->
            merge_props(Rest, [lists:keydelete(props, 1, R)++Term|Acc]);
        Term when is_map(Term) ->
            T1 = lists:map(
                fun({K,V}) ->
                    {z_convert:to_atom(K), V}
                end,
                maps:to_list(Term)),
            merge_props(Rest, [lists:keydelete(props, 1, R)++T1|Acc])
    end.


-spec assoc1(atom(), z:context(), sql(), parameters(), pos_integer()) -> {ok, [proplists:proplist()]}.
assoc1(DbDriver, C, Sql, Parameters, Timeout) ->
    case DbDriver:equery(C, Sql, Parameters, Timeout) of
        {ok, Columns, Rows} ->
            Names = [ list_to_atom(binary_to_list(Name)) || #column{name=Name} <- Columns ],
            Rows1 = [ lists:zip(Names, tuple_to_list(Row)) || Row <- Rows ],
            {ok, Rows1};
        Other -> Other
    end.


equery1(DbDriver, C, Sql) ->
    equery1(DbDriver, C, Sql, [], ?TIMEOUT).

equery1(DbDriver, C, Sql, Parameters) when is_list(Parameters); is_tuple(Parameters) ->
    equery1(DbDriver, C, Sql, Parameters, ?TIMEOUT).

equery1(DbDriver, C, Sql, Parameters, Timeout) ->
    case DbDriver:equery(C, Sql, Parameters, Timeout) of
        {ok, _Columns, []} -> {error, noresult};
        {ok, _RowCount, _Columns, []} -> {error, noresult};
        {ok, _Columns, [Row|_]} -> {ok, element(1, Row)};
        {ok, _RowCount, _Columns, [Row|_]} -> {ok, element(1, Row)};
        Other -> Other
    end.


-ifdef(TEST).

prepare_cols_test() ->
    ?assertEqual({ok, #{}}, prepare_cols([], #{})),

    % Props go to the right place
    ?assertEqual({ok, #{<<"a">> => <<"a value">>}},
                 prepare_cols([<<"a">>, <<"b">>], #{<<"a">> => <<"a value">>})),
    ?assertEqual({ok, #{<<"a">> => <<"a value">>, <<"b">> => <<"b value">>}},
                 prepare_cols([<<"a">>, <<"b">>], #{<<"a">> => <<"a value">>,
                                                    <<"b">> => <<"b value">>})),

    % Column is not known
    ?assertEqual({error, {unknown_column,[<<"c">>]}},
                 prepare_cols([<<"a">>, <<"b">>], #{<<"a">> => <<"a value">>,
                                                    <<"c">> => <<"c value">>})),

    % When there is a props column, unknown properties go to that column.
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props">> => #{<<"c">> => <<"c value">>}}},
                 prepare_cols([<<"a">>, <<"b">>, <<"props">>], #{<<"a">> => <<"a value">>,
                                                                 <<"c">> => <<"c value">>})),

    % An existing props map will be merged with any new values.
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props">> => #{<<"c">> => <<"c value">>,
                                        <<"d">> => <<"d value">>}}},
                 prepare_cols([<<"a">>, <<"b">>, <<"props">>],
                              #{<<"a">> => <<"a value">>,
                                <<"c">> => <<"c value">>,
                                <<"props">> => #{<<"d">> => <<"d value">>}})),

    % When there is a props_json column, unknown properties go to that column.
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>}}},
                 prepare_cols([<<"a">>, <<"b">>, <<"props">>, <<"props_json">>],
                              #{<<"a">> => <<"a value">>,
                                <<"c">> => <<"c value">>})),

    % When there is a props_json column, that gets priority
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>}}},
                 prepare_cols([<<"a">>, <<"b">>, <<"props">>, <<"props_json">>],
                              #{<<"a">> => <<"a value">>, <<"c">> => <<"c value">>})),
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>}}},
                 prepare_cols([<<"a">>, <<"b">>, <<"props_json">>],
                              #{<<"a">> => <<"a value">>, <<"c">> => <<"c value">>})),

    % existing props and props_json fields are merged
    ?assertEqual({ok,#{<<"a">> => <<"a value">>,
                       <<"props_json">> => #{<<"c">> => <<"c value">>,
                                             <<"e">> => <<"e value">>}}},
                 prepare_cols([<<"a">>, <<"b">>, <<"props">>, <<"props_json">>],
                              #{<<"a">> => <<"a value">>,
                                <<"c">> => <<"c value">>,
                                <<"props_json">> => #{<<"e">> => <<"e value">>}
                               })),


    ok.


-endif.
