%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2014 Arjan Scherpenisse
%% Date: 2014-04-29
%%
%% @doc Postgresql pool worker

%% Copyright 2014 Arjan Scherpenisse
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

-module(z_db_pgsql).
-behaviour(gen_server).

-behaviour(poolboy_worker).
-behaviour(z_db_worker).

-include("zotonic.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% poolboy_worker callbacks
-export([start_link/1]).

%% z_db_worker callbacks
-export([
         test_connection/1,
         squery/3,
         equery/4,
         get_raw_connection/1
        ]).

-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).

-define(IDLE_TIMEOUT, 60000).

-define(CONNECT_RETRIES, 50).
-define(CONNECT_RETRY_SLEEP, 10000).
-define(CONNECT_RETRY_SHORT, 10).

-record(state, {conn, conn_args}).


%%
%% API
%%

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

test_connection(Args) ->
    case connect(Args) of
        {ok, Conn} ->
            case z_db:schema_exists_conn(Conn, proplists:get_value(dbschema, Args, "public")) of
                true ->
                    epgsql:close(Conn),
                    ok;
                false ->
                    epgsql:close(Conn),
                    {error, noschema}
            end;
        {error, _} = E ->
            E
    end.

squery(Worker, Sql, Timeout) ->
    gen_server:call(Worker, {squery, Sql}, Timeout).

equery(Worker, Sql, Parameters, Timeout) ->
    gen_server:call(Worker, {equery, Sql, Parameters}, Timeout).


%% This function should not be used but currently is required by the
%% install / upgrade routines. Can only be called from inside a
%% z_db:transaction/2.
get_raw_connection(#context{dbc=Worker}) when Worker =/= undefined ->
    gen_server:call(Worker, get_raw_connection).


%%
%% gen_server callbacks
%%

init(Args) ->
    %% Start disconnected
    process_flag(trap_exit, true),
    {ok, #state{conn=undefined, conn_args=Args}, ?IDLE_TIMEOUT}.


handle_call(Cmd, _From, #state{conn=undefined, conn_args=Args}=State) ->
    case connect(Args) of
        {ok, Conn} ->
            handle_call(Cmd, _From, State#state{conn=Conn});
        {error, _} = E ->
            {reply, E, State}
    end;

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, decode_reply(epgsql:squery(Conn, Sql)), State, ?IDLE_TIMEOUT};


handle_call({equery, Sql, Params}, _From, #state{conn=Conn}=State) ->
    {reply, decode_reply(epgsql:equery(Conn, Sql, encode_values(Params))), State, ?IDLE_TIMEOUT};

handle_call(get_raw_connection, _From, #state{conn=Conn}=State) ->
    {reply, Conn, State, ?IDLE_TIMEOUT};

handle_call(_Request, _From, State) ->
    {reply, unknown_call, State, ?IDLE_TIMEOUT}.


handle_cast(_Msg, State) ->
    {noreply, State, ?IDLE_TIMEOUT}.

handle_info(disconnect, #state{conn=undefined} = State) ->
    {noreply, State};
handle_info(disconnect, State) ->
    Database = get_arg(dbdatabase, State#state.conn_args),
    Schema = get_arg(dbschema, State#state.conn_args),
    lager:debug("Closing connection to ~s/~s (~p)", [Database, Schema, self()]),
    {noreply, disconnect(State)};
handle_info(timeout, State) ->
    {noreply, disconnect(State)};
handle_info({'EXIT', Pid, _Reason}, #state{conn=Pid} = State) ->
    % Unexpected EXIT from the connection
    {noreply, State#state{conn=undefined}};
handle_info({'EXIT', _Pid, _Reason}, #state{conn=undefined} = State) ->
    % Expected EXIT after disconnect
    {noreply, State, hibernate};
handle_info(_Info, State) ->
    {noreply, State, ?IDLE_TIMEOUT}.

terminate(_Reason, #state{conn=undefined}) ->
    ok;
terminate(_Reason, #state{conn=Conn}) ->
    _ = epgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Helper functions
%%
connect(Args) when is_list(Args) ->
    connect(Args, 0).

connect(_Args, RetryCt) when RetryCt >= ?CONNECT_RETRIES ->
    {error, econnrefused};
connect(Args, RetryCt) ->
    Hostname = get_arg(dbhost, Args),
    Port = get_arg(dbport, Args),
    Database = get_arg(dbdatabase, Args),
    Username = get_arg(dbuser, Args),
    Password = get_arg(dbpassword, Args),
    Schema = get_arg(dbschema, Args),
    try
        case epgsql:connect(Hostname, Username, Password,
                           [{database, Database}, {port, Port}]) of
            {ok, Conn} ->
                case epgsql:squery(Conn, "SET TIME ZONE 'UTC'; SET search_path TO " ++ Schema) of
                    [{ok, [], []}, {ok, [], []}] ->
                        {ok, Conn};
                    Error ->
                        catch epgsql:close(Conn),
                        {error, Error}
                end;
            {error, econnrefused} ->
                lager:warning("psql connection to ~p:~p refused (econnrefused), retrying in ~p sec (~p)",
                              [Hostname, Port, ?CONNECT_RETRY_SLEEP div 1000, self()]),
                timer:sleep(?CONNECT_RETRY_SLEEP),
                connect(Args, RetryCt+10);
            {error, {error,fatal,<<"53300">>,_ErrorMsg,_ErrorArgs}} ->
                too_many_connections(Args, RetryCt);
            {error, <<"53300">>} ->
                too_many_connections(Args, RetryCt);
            {error, _} = E ->
                lager:warning("psql connection to ~p:~p returned error ~p",
                              [Hostname, Port, E]),
                E
        end
    catch
        A:B ->
            lager:error("psql connection to ~p:~p failed (exception ~p:~p), retrying in ~p sec (~p)",
                        [Hostname, Port, A, B, ?CONNECT_RETRY_SLEEP div 1000, self()]),
            timer:sleep(?CONNECT_RETRY_SLEEP),
            connect(Args, RetryCt+1)
    end.

too_many_connections(Args, RetryCt) ->
    Hostname = get_arg(dbhost, Args),
    Port = get_arg(dbport, Args),
    lager:warning("psql connection to ~p:~p refused (too many connections), retrying in ~p msec (~p)",
                  [Hostname, Port, ?CONNECT_RETRY_SHORT, self()]),
    z_db_pool:close_connections(),
    timer:sleep(?CONNECT_RETRY_SHORT),
    connect(Args, RetryCt+1).


disconnect(#state{conn=undefined} = State) ->
    State;
disconnect(#state{conn=Conn} = State) ->
    _ = epgsql:close(Conn),
    State#state{conn=undefined}.

get_arg(K, Args) ->
    maybe_default(K, proplists:get_value(K, Args)).

maybe_default(dbport, 0) -> z_config:get(dbport);
maybe_default(K, undefined) -> z_config:get(K);
maybe_default(K, "") -> z_config:get(K);
maybe_default(K, <<>>) -> z_config:get(K);
maybe_default(_K, V) -> V.


%%
%% These are conversion routines between how z_db expects values and how epgsl expects them.

%% Notable differences:
%% - Input values {term, ...} (use the ?DB_PROPS(...) macro!) are term_to_binary encoded and decoded
%% - null <-> undefined
%% - date/datetimes have a floating-point second argument in epgsql, in Zotonic they don't.

encode_values(L) when is_list(L) ->
    lists:map(fun encode_value/1, L).

encode_value(undefined) ->
    null;
encode_value({term, undefined}) ->
    null;
encode_value({term, Term}) ->
    B = term_to_binary(Term),
    <<?TERM_MAGIC_NUMBER, B/binary>>;
encode_value(Value) ->
    Value.


decode_reply({ok, Columns, Rows}) ->
    {ok, Columns, lists:map(fun decode_values/1, Rows)};
decode_reply({ok, Nr, Columns, Rows}) ->
    {ok, Nr, Columns, lists:map(fun decode_values/1, Rows)};
decode_reply(R) ->
    R.

decode_values(T) when is_tuple(T) ->
    list_to_tuple(decode_values(tuple_to_list(T)));
decode_values(L) when is_list(L) ->
    lists:map(fun decode_value/1, L).

decode_value({V}) ->
    {decode_value(V)};

decode_value(null) ->
    undefined;
decode_value(<<?TERM_MAGIC_NUMBER, B/binary>>) ->
    binary_to_term(B);
decode_value({H,M,S}) when is_float(S) ->
    {H,M,trunc(S)};
decode_value({{Y,Mm,D},{H,M,S}}) when is_float(S) ->
    {{Y,Mm,D},{H,M,trunc(S)}};
decode_value(V) ->
    V.
