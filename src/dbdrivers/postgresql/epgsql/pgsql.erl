%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.
%%% Copyright (C) 2009-2011 - MARC WORRELL.  All rights reserved.

%% Modified by Marc Worrell, 2009.
%% Added last_id/2, reset_id/2, squery1/2, equery1/2, equery1/3, assoc/2, assoc/3, columns/2
%% Adapted with_transaction/2

-module(pgsql).

-export([
    connect/2, connect/3, connect/4, connect/5,
    close/1
]).
-export([
        last_id/2, last_id/3, 
        reset_id/2, reset_id/3, 
        squery1/2, squery1/3, 
        equery1/2, equery1/3, equery1/4,
        assoc/2, assoc/3, assoc/4
]).
-export([
    get_parameter/2, 
    squery/2, squery/3, 
    equery/2, equery/3, equery/4
]).
-export([parse/2, parse/3, parse/4, describe/2, describe/3]).
-export([bind/3, bind/4, execute/2, execute/3, execute/4]).
-export([close/2, close/3, sync/1]).
-export([with_transaction/2]).

-include("pgsql.hrl").
-include("zotonic.hrl").

-define(TIMEOUT, 50000).

%% -- client interface --

connect(Host, Opts) ->
    connect(Host, os:getenv("USER"), "", Opts).

connect(Host, Username, Opts) ->
    connect(Host, Username, "", Opts).
connect(Host, Username, Password, Opts) ->
    connect(Host, Username, Password, Opts, ?TIMEOUT).

connect(Host, Username, Password, Opts, Timeout) ->
    {ok, C} = pgsql_connection:start_link(),
    case pgsql_connection:connect(C, Host, Username, Password, Opts, Timeout) of
        {ok, Conn} ->
            case proplists:get_value(schema, Opts) of
                undefined -> 
                    {ok, Conn};
                Schema when is_list(Schema) ->
                    case squery(Conn, "SET search_path TO " ++ Schema, Timeout) of
                        {ok, [], []} ->
                            {ok, Conn};
                        Error -> 
                            close(Conn),
                            Error
                    end
            end;
        Error -> 
            Error
    end.

close(C) when is_pid(C) ->
    pgsql_connection:stop(C),
    ok.

get_parameter(C, Name) ->
    pgsql_connection:get_parameter(C, Name).

last_id(C, Table) ->
    last_id(C, Table, ?TIMEOUT).
    
last_id(C, Table, Timeout) when is_atom(Table) ->
    last_id(C, atom_to_list(Table), Timeout);
last_id(C, Table, Timeout) ->
    equery1(C, "select currval(pg_get_serial_sequence($1, 'id'))", [Table], Timeout).

reset_id(C, Table) ->
    reset_id(C, Table, ?TIMEOUT).
    
reset_id(C, Table, Timeout) when is_atom(Table) ->
    reset_id(C, atom_to_list(Table), Timeout);
reset_id(C, Table, Timeout) ->
    {ok, Max} = equery1(C, "select max(id) from \""++Table++"\"", Timeout),
    equery1(C, "select setval(pg_get_serial_sequence($1, 'id'), $2)", [Table, Max+1], Timeout).

assoc(C, Sql) ->
    assoc(C, Sql, [], ?TIMEOUT).

assoc(C, Sql, Timeout) when is_integer(Timeout) ->
    assoc(C, Sql, [], Timeout);
assoc(C, Sql, Parameters) when is_list(Parameters); is_tuple(Parameters) ->
    assoc(C, Sql, Parameters, ?TIMEOUT).
    
assoc(C, Sql, Parameters, Timeout) ->
    case equery(C, Sql, Parameters, Timeout) of
        {ok, Columns, Rows} ->
            Names = [ list_to_atom(binary_to_list(Name)) || #column{name=Name} <- Columns ],
            Rows1 = [ lists:zip(Names, tuple_to_list(Row)) || Row <- Rows ],
            {ok, Rows1};
        Other -> Other
    end.


squery1(C, Sql) ->
    squery1(C, Sql, ?TIMEOUT).

squery1(C, Sql, Timeout) ->
    case squery(C,Sql, Timeout) of
        {ok, _Columns, []} -> {error, noresult};
        {ok, _RowCount, _Columns, []} -> {error, noresult};
        {ok, _Columns, [Row|_]} -> {ok, element(1, Row)};
        {ok, _RowCount, _Columns, [Row|_]} -> {ok, element(1, Row)};
        Other -> Other
    end.

equery1(C, Sql) ->
    equery1(C, Sql, [], ?TIMEOUT).

equery1(C, Sql, Parameters) when is_list(Parameters); is_tuple(Parameters) ->
    equery1(C, Sql, Parameters, ?TIMEOUT);
equery1(C, Sql, Timeout) when is_integer(Timeout) ->
    equery1(C, Sql, [], Timeout).

equery1(C, Sql, Parameters, Timeout) ->
    case equery(C, Sql, Parameters, Timeout) of
        {ok, _Columns, []} -> {error, noresult};
        {ok, _RowCount, _Columns, []} -> {error, noresult};
        {ok, _Columns, [Row|_]} -> {ok, element(1, Row)};
        {ok, _RowCount, _Columns, [Row|_]} -> {ok, element(1, Row)};
        Other -> Other
    end.


    
squery(C, Sql) ->
    squery(C, Sql, ?TIMEOUT).

squery(C, Sql, Timeout) ->
    ok = pgsql_connection:squery(C, Sql, Timeout),
    case receive_results(C, [], Timeout) of
        [Result] -> Result;
        Results  -> Results
    end.

equery(C, Sql) ->
    equery(C, Sql, [], ?TIMEOUT).

equery(C, Sql, Timeout) when is_integer(Timeout) ->
    equery(C, Sql, [], Timeout);
equery(C, Sql, Parameters) when is_tuple(Parameters) ->
    equery(C, Sql, tuple_to_list(Parameters), ?TIMEOUT);
equery(C, Sql, Parameters) when is_list(Parameters) ->
    equery(C, Sql, Parameters, ?TIMEOUT).

equery(C, Sql, Parameters, Timeout) when is_tuple(Parameters) ->
    equery(C, Sql, tuple_to_list(Parameters), Timeout);
equery(C, Sql, Parameters, Timeout) when is_list(Parameters) ->
    case pgsql_connection:parse(C, "", Sql, [], Timeout) of
        {ok, #statement{types = Types} = S} ->
            Typed_Parameters = lists:zip(Types, Parameters),
            ok = pgsql_connection:equery(C, S, Typed_Parameters, Timeout),
            receive_result(C, Timeout);
        Error ->
            ?LOG("SQL error ~p : ~p", [Error, Sql]),
            Error
    end.

%% parse

parse(C, Sql) ->
    parse(C, "", Sql, []).

parse(C, Sql, Types) ->
    parse(C, "", Sql, Types).

parse(C, Name, Sql, Types) ->
    pgsql_connection:parse(C, Name, Sql, Types).

%% bind

bind(C, Statement, Parameters) ->
    bind(C, Statement, "", Parameters).

bind(C, Statement, PortalName, Parameters) ->
    pgsql_connection:bind(C, Statement, PortalName, Parameters).

%% execute

execute(C, S) ->
    execute(C, S, "", 0).

execute(C, S, N) ->
    execute(C, S, "", N).

execute(C, S, PortalName, N) ->
    pgsql_connection:execute(C, S, PortalName, N),
    receive_extended_result(C).

%% statement/portal functions

describe(C, #statement{name = Name}) ->
    pgsql_connection:describe(C, statement, Name).

describe(C, Type, Name) ->
    pgsql_connection:describe(C, Type, Name).

close(C, #statement{name = Name}) ->
    pgsql_connection:close(C, statement, Name).

close(C, Type, Name) ->
    pgsql_connection:close(C, Type, Name).

sync(C) ->
    pgsql_connection:sync(C).

%% misc helper functions
with_transaction(C, F) ->
    try
        {ok, [], []} = squery(C, "BEGIN"),
        R = F(C),
        {ok, [], []} = squery(C, "COMMIT"),
        R
    catch
        E:Why ->
            ?LOG("Exception in transaction: \"~p,~p\"", [E,Why]),
            squery(C, "ROLLBACK"),
            {rollback, Why}
    end.

%% -- internal functions --

receive_result(C, Timeout) ->
    R = receive_result(C, [], [], Timeout),
    receive
        {pgsql, C, done} -> R
    end.

receive_results(C, Results, Timeout) ->
    case receive_result(C, [], [], Timeout) of
        done -> lists:reverse(Results);
        R    -> receive_results(C, [R | Results], Timeout)
    end.

receive_result(C, Cols, Rows, Timeout) ->
    receive
        {pgsql, C, {columns, Cols2}} ->
            receive_result(C, Cols2, Rows, Timeout);
        {pgsql, C, {data, Row}} ->
            receive_result(C, Cols, [Row | Rows], Timeout);
        {pgsql, C, {error, _E} = Error} ->
            Error;
        {pgsql, C, {complete, {_Type, Count}}} ->
            case Rows of
                [] -> {ok, Count};
                _L -> {ok, Count, Cols, lists:reverse(Rows)}
            end;
        {pgsql, C, {complete, _Type}} ->
            {ok, Cols, lists:reverse(Rows)};
        {pgsql, C, {notice, _N}} ->
            receive_result(C, Cols, Rows, Timeout);
        {pgsql, C, done} ->
            done
    after
        Timeout -> {error, timeout}
    end.

receive_extended_result(C)->
    receive_extended_result(C, []).

receive_extended_result(C, Rows) ->
    receive_extended_result(C, Rows, ?TIMEOUT).

receive_extended_result(C, Rows, Timeout) ->
    receive
        {pgsql, C, {data, Row}} ->
            receive_extended_result(C, [Row | Rows]);
        {pgsql, C, {error, _E} = Error} ->
            Error;
        {pgsql, C, suspended} ->
            {partial, lists:reverse(Rows)};
        {pgsql, C, {complete, {_Type, Count}}} ->
            case Rows of
                [] -> {ok, Count};
                _L -> {ok, Count, lists:reverse(Rows)}
            end;
        {pgsql, C, {complete, _Type}} ->
            {ok, lists:reverse(Rows)};
        {pgsql, C, {notice, _N}} ->
            receive_extended_result(C, Rows)
    after
        Timeout -> {error, timeout}
    end.
