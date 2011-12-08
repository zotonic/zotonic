%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.
%%% Copyright (C) 2009-2011 - Marc Worrell.  All rights reserved.

-module(pgsql_connection).

-behavior(gen_fsm).

-export([start_link/0, stop/1, connect/5, get_parameter/2]).
-export([squery/2, equery/3]).
-export([parse/4, bind/4, execute/4, describe/3]).
-export([close/3, sync/1]).
-export([database/1]).

-export([init/1, handle_event/3, handle_sync_event/4]).
-export([handle_info/3, terminate/3, code_change/4]).
-export([read/3]).

-export([startup/3, auth/2, initializing/2, ready/2, ready/3]).
-export([querying/2, parsing/2, binding/2, describing/2]).
-export([executing/2, closing/2, synchronizing/2]).

-include("pgsql.hrl").
-include("zotonic.hrl").

-record(state, {
          async,
          reader,
          sock,
          parameters = [],
          integer_datetimes = false,
          database,
          reply,
          reply_to,
          backend,
          statement,
          txstatus}).

-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).

%% -- client interface --

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

stop(C) ->
    gen_fsm:send_all_state_event(C, stop).

connect(C, Host, Username, Password, Opts) ->
    gen_fsm:sync_send_event(C, {connect, Host, Username, Password, Opts}, ?PGSQL_TIMEOUT).

get_parameter(C, Name) ->
    gen_fsm:sync_send_event(C, {get_parameter, to_binary(Name)}, ?PGSQL_TIMEOUT).

squery(C, Sql) ->
    gen_fsm:sync_send_event(C, {squery, Sql}, ?PGSQL_TIMEOUT).

equery(C, Statement, Parameters) ->
    gen_fsm:sync_send_event(C, {equery, Statement, Parameters}, ?PGSQL_TIMEOUT).

parse(C, Name, Sql, Types) ->
    gen_fsm:sync_send_event(C, {parse, Name, Sql, Types}, ?PGSQL_TIMEOUT).

bind(C, Statement, PortalName, Parameters) ->
    gen_fsm:sync_send_event(C, {bind, Statement, PortalName, Parameters}, ?PGSQL_TIMEOUT).

execute(C, Statement, PortalName, MaxRows) ->
    gen_fsm:sync_send_event(C, {execute, Statement, PortalName, MaxRows}, ?PGSQL_TIMEOUT).

describe(C, Type, Name) ->
    gen_fsm:sync_send_event(C, {describe, Type, Name}, ?PGSQL_TIMEOUT).

close(C, Type, Name) ->
    gen_fsm:sync_send_event(C, {close, Type, Name}, ?PGSQL_TIMEOUT).

sync(C) ->
    gen_fsm:sync_send_event(C, sync, ?PGSQL_TIMEOUT).
    
database(C) ->
    gen_fsm:sync_send_event(C, database, ?PGSQL_TIMEOUT).

%% -- gen_fsm implementation --

init([]) ->
    process_flag(trap_exit, true),
    {ok, startup, #state{}}.

handle_event({notice, Notice}, State_Name, State) ->
    notify(State, {notice, Notice}),
    {next_state, State_Name, State};

handle_event({notification, _Channel, _Pid, _Payload} = Msg, State_Name, State) ->
    notify_async(State, Msg),
    {next_state, State_Name, State};

handle_event({parameter_status, Name, Value}, State_Name, State) ->
    State1 = case {Name, Value} of
        {<<"integer_datetimes">>, <<"on">>} -> State#state{integer_datetimes=true};
        {<<"integer_datetimes">>, <<"off">>} -> State#state{integer_datetimes=false};
        _ -> State
    end,
    Parameters2 = lists:keystore(Name, 1, State1#state.parameters, {Name, Value}),
    {next_state, State_Name, State1#state{parameters = Parameters2}};

handle_event(stop, _State_Name, State) ->
    {stop, normal, State};

handle_event(Event, _State_Name, State) ->
    {stop, {unsupported_event, Event}, State}.

handle_sync_event(Event, _From, _State_Name, State) ->
    {stop, {unsupported_sync_event, Event}, State}.

handle_info({'EXIT', Pid, Reason}, _State_Name, State = #state{reader = Pid}) ->
    {stop, Reason, State};

handle_info(Info, _State_Name, State) ->
    {stop, {unsupported_info, Info}, State}.

terminate(_Reason, _State_Name, State = #state{sock = Sock})
  when Sock =/= undefined ->
    send(State, $X, []),
    gen_tcp:close(Sock);

terminate(_Reason, _State_Name, _State) ->
    ok.

code_change(_Old_Vsn, State_Name, State, _Extra) ->
    {ok, State_Name, State}.

%% -- states --

startup({connect, Host, Username, Password, Opts}, From, State) ->
    Port      = proplists:get_value(port, Opts, 5432),
    Sock_Opts = [{nodelay, true}, {active, false}, {packet, raw}, binary],
    Async   = proplists:get_value(async, Opts, undefined),
    case gen_tcp:connect(Host, Port, Sock_Opts) of
        {ok, Sock} ->
            Reader = spawn_link(?MODULE, read, [self(), Sock, <<>>]),

            Opts2 = ["user", 0, Username, 0],
            case proplists:get_value(database, Opts, undefined) of
                undefined -> Opts3 = Opts2;
                Database  -> Opts3 = [Opts2 | ["database", 0, Database, 0]]
            end,

            put(username, Username),
            put(password, Password),
            State2 = State#state{reader   = Reader,
                                 sock     = Sock,
                                 reply_to = From,
                                 async    = Async,
                                 database = proplists:get_value(database, Opts, undefined)},
            send(State2, [<<196608:32>>, Opts3, 0]),

            {next_state, auth, State2};
        Error ->
            {stop, normal, Error, State}
    end.

%% AuthenticationOk
auth({$R, <<0:?int32>>}, State) ->
    {next_state, initializing, State};

%% AuthenticationCleartextPassword
auth({$R, <<3:?int32>>}, State) ->
    send(State, $p, [get(password), 0]),
    {next_state, auth, State};

%% AuthenticationMD5Password
auth({$R, <<5:?int32, Salt:4/binary>>}, State) ->
    Digest1 = hex(erlang:md5([get(password), get(username)])),
    Str = ["md5", hex(erlang:md5([Digest1, Salt])), 0],
    send(State, $p, Str),
    {next_state, auth, State};

auth({$R, <<M:?int32, _/binary>>}, State) ->
    case M of
        2 -> Method = kerberosV5;
        4 -> Method = crypt;
        6 -> Method = scm;
        7 -> Method = gss;
        8 -> Method = sspi;
        _ -> Method = unknown
    end,
    Error = {error, {unsupported_auth_method, Method}},
    gen_fsm:reply(State#state.reply_to, Error),
    {stop, normal, State};

%% ErrorResponse
auth({$E, Bin}, State) ->
    Error = decode_error(Bin),
    case Error#error.code of
        <<"28000">> -> Why = invalid_authorization_specification;
        Any         -> Why = Any
    end,
    gen_fsm:reply(State#state.reply_to, {error, Why}),
    {stop, normal, State}.

%% BackendKeyData
initializing({$K, <<Pid:?int32, Key:?int32>>}, State) ->
    State2 = State#state{backend = {Pid, Key}},
    {next_state, initializing, State2};

%% ErrorResponse
initializing({$E, Bin}, State) ->
    Error = decode_error(Bin),
    case Error#error.code of
        <<"28000">> -> Why = invalid_authorization_specification;
        Any         -> Why = Any
    end,
    gen_fsm:reply(State#state.reply_to, {error, Why}),
    {stop, normal, State};

%% ReadyForQuery
initializing({$Z, <<Status:8>>}, State) ->
    erase(username),
    erase(password),
    gen_fsm:reply(State#state.reply_to, {ok, self()}),
    {next_state, ready, State#state{txstatus = Status}}.

ready(_Msg, State) ->
    {next_state, ready, State}.

%% execute simple query
ready({squery, Sql}, From, State) ->
    send(State, $Q, [Sql, 0]),
    State2 = State#state{statement = #statement{}, reply_to = From},
    {reply, ok, querying, State2};

%% execute extended query
ready({equery, Statement, Parameters}, From, State) ->
    #statement{name = StatementName, columns = Columns} = Statement,
    Bin1 = encode_parameters(Parameters, State#state.integer_datetimes),
    Bin2 = encode_formats(Columns),
    send(State, $B, ["", 0, StatementName, 0, Bin1, Bin2]),
    send(State, $E, ["", 0, <<0:?int32>>]),
    send(State, $C, [$S, "", 0]),
    send(State, $S, []),
    State2 = State#state{statement = Statement, reply_to = From},
    {reply, ok, querying, State2};

ready({get_parameter, Name}, _From, State) ->
    case lists:keysearch(Name, 1, State#state.parameters) of
        {value, {Name, Value}} -> Value;
        false                  -> Value = undefined
    end,
    {reply, {ok, Value}, ready, State};

ready(database, _From, State) ->
    {reply, {ok, State#state.database}, ready, State};

ready({parse, Name, Sql, Types}, From, State) ->
    Bin = encode_types(Types),
    send(State, $P, [Name, 0, Sql, 0, Bin]),
    send(State, $D, [$S, Name, 0]),
    send(State, $H, []),
    S = #statement{name = Name},
    {next_state, parsing, State#state{statement = S, reply_to = From}};

ready({bind, Statement, PortalName, Parameters}, From, State) ->
    #statement{name = StatementName, columns = Columns, types = Types} = Statement,
    Typed_Parameters = lists:zip(Types, Parameters),
    Bin1 = encode_parameters(Typed_Parameters, State#state.integer_datetimes),
    Bin2 = encode_formats(Columns),
    send(State, $B, [PortalName, 0, StatementName, 0, Bin1, Bin2]),
    send(State, $H, []),
    {next_state, binding, State#state{statement = Statement, reply_to = From}};

ready({execute, Statement, PortalName, MaxRows}, From, State) ->
    send(State, $E, [PortalName, 0, <<MaxRows:?int32>>]),
    send(State, $H, []),
    {reply, ok, executing, State#state{statement = Statement, reply_to = From}};

ready({describe, Type, Name}, From, State) ->
    case Type of
        statement -> Type2 = $S;
        portal    -> Type2 = $P
    end,
    send(State, $D, [Type2, Name, 0]),
    send(State, $H, []),
    {next_state, describing, State#state{reply_to = From}};

ready({close, Type, Name}, From, State) ->
    case Type of
        statement -> Type2 = $S;
        portal    -> Type2 = $P
    end,
    send(State, $C, [Type2, Name, 0]),
    send(State, $H, []),
    {next_state, closing, State#state{reply_to = From}};

ready(sync, From, State) ->
    send(State, $S, []),
    {next_state, synchronizing, State#state{reply = ok, reply_to = From}}.

%% BindComplete
querying({$2, <<>>}, State) ->
    #state{statement = #statement{columns = Columns}} = State,
    notify(State, {columns, Columns}),
    {next_state, querying, State};

%% CloseComplete
querying({$3, <<>>}, State) ->
    {next_state, querying, State};

%% RowDescription
querying({$T, <<Count:?int16, Bin/binary>>}, State) ->
    Columns = decode_columns(Count, Bin),
    S2 = (State#state.statement)#statement{columns = Columns},
    notify(State, {columns, Columns}),
    {next_state, querying, State#state{statement = S2}};

%% DataRow
querying({$D, <<_Count:?int16, Bin/binary>>}, State) ->
    #state{statement = #statement{columns = Columns}} = State,
    Data = decode_data(Columns, Bin, State#state.integer_datetimes),
    notify(State, {data, Data}),
    {next_state, querying, State};

%% CommandComplete
querying({$C, Bin}, State) ->
    Complete = decode_complete(Bin),
    notify(State, {complete, Complete}),
    {next_state, querying, State};

%% EmptyQueryResponse
querying({$I, _Bin}, State) ->
    notify(State, {complete, empty}),
    {next_state, querying, State};

%% ErrorResponse
querying({$E, Bin}, State) ->
    Error = decode_error(Bin),
    notify(State, {error, Error}),
    {next_state, querying, State};

%% ReadyForQuery
querying({$Z, <<_Status:8>>}, State) ->
    notify(State, done),
    {next_state, ready, State#state{reply_to = undefined}}.

%% ParseComplete
parsing({$1, <<>>}, State) ->
    {next_state, describing, State};

%% ErrorResponse
parsing({$E, Bin}, State) ->
    Reply = {error, decode_error(Bin)},
    send(State, $S, []),
    {next_state, parsing, State#state{reply = Reply}};

%% ReadyForQuery
parsing({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% BindComplete
binding({$2, <<>>}, State) ->
    gen_fsm:reply(State#state.reply_to, ok),
    {next_state, ready, State};

%% ErrorResponse
binding({$E, Bin}, State) ->
    Reply = {error, decode_error(Bin)},
    send(State, $S, []),
    {next_state, binding, State#state{reply = Reply}};

%% ReadyForQuery
binding({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% ParameterDescription
describing({$t, <<_Count:?int16, Bin/binary>>}, State) ->
    Types = [pgsql_types:oid2type(Oid) || <<Oid:?int32>> <= Bin],
    S2 = (State#state.statement)#statement{types = Types},
    {next_state, describing, State#state{statement = S2}};

%% RowDescription
describing({$T, <<Count:?int16, Bin/binary>>}, State) ->
    Columns = decode_columns(Count, Bin),
    Columns2 = [C#column{format = format(C#column.type)} || C <- Columns],
    S2 = (State#state.statement)#statement{columns = Columns2},
    gen_fsm:reply(State#state.reply_to, {ok, S2}),
    {next_state, ready, State};

%% NoData
describing({$n, <<>>}, State) ->
    S2 = (State#state.statement)#statement{columns = []},
    gen_fsm:reply(State#state.reply_to, {ok, S2}),
    {next_state, ready, State};

%% ErrorResponse
describing({$E, Bin}, State) ->
    Reply = {error, decode_error(Bin)},
    send(State, $S, []),
    {next_state, describing, State#state{reply = Reply}};

%% ReadyForQuery
describing({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% DataRow
executing({$D, <<_Count:?int16, Bin/binary>>}, State) ->
    #state{statement = #statement{columns = Columns}} = State,
    Data = decode_data(Columns, Bin, State#state.integer_datetimes),
    notify(State, {data, Data}),
    {next_state, executing, State};

%% PortalSuspended
executing({$s, <<>>}, State) ->
    notify(State, suspended),
    {next_state, ready, State};

%% CommandComplete
executing({$C, Bin}, State) ->
    notify(State, {complete, decode_complete(Bin)}),
    {next_state, ready, State};

%% EmptyQueryResponse
executing({$I, _Bin}, State) ->
    notify(State, {complete, empty}),
    {next_state, ready, State};

%% ErrorResponse
executing({$E, Bin}, State) ->
    notify(State, {error, decode_error(Bin)}),
    {next_state, executing, State}.

%% CloseComplete
closing({$3, <<>>}, State) ->
    gen_fsm:reply(State#state.reply_to, ok),
    {next_state, ready, State};

%% ErrorResponse
closing({$E, Bin}, State) ->
    Error = {error, decode_error(Bin)},
    gen_fsm:reply(State#state.reply_to, Error),
    {next_state, ready, State}.

%% ErrorResponse
synchronizing({$E, Bin}, State) ->
    Reply = {error, decode_error(Bin)},
    {next_state, synchronizing, State#state{reply = Reply}};

%% ReadyForQuery
synchronizing({$Z, <<Status:8>>}, State) ->
    #state{reply = Reply, reply_to = Reply_To} = State,
    gen_fsm:reply(Reply_To, Reply),
    {next_state, ready, State#state{reply = undefined, txstatus = Status}}.

%% -- internal functions --

%% decode a single null-terminated string
decode_string(Bin) ->
    decode_string(Bin, <<>>).

decode_string(<<0, Rest/binary>>, Str) ->
    {Str, Rest};
decode_string(<<C, Rest/binary>>, Str) ->
    decode_string(Rest, <<Str/binary, C>>).

%% decode multiple null-terminated string
decode_strings(Bin) ->
    decode_strings(Bin, []).

decode_strings(<<>>, Acc) ->
    lists:reverse(Acc);
decode_strings(Bin, Acc) ->
    {Str, Rest} = decode_string(Bin),
    decode_strings(Rest, [Str | Acc]).

%% decode field
decode_fields(Bin) ->
    decode_fields(Bin, []).

decode_fields(<<0>>, Acc) ->
    Acc;
decode_fields(<<Type:8, Rest/binary>>, Acc) ->
    {Str, Rest2} = decode_string(Rest),
    decode_fields(Rest2, [{Type, Str} | Acc]).

%% decode data
decode_data(Columns, Bin, IntegerDatetime) ->
    decode_data(Columns, Bin, IntegerDatetime, []).

decode_data([], _Bin, _IntegerDatetime, Acc) ->
    list_to_tuple(lists:reverse(Acc));
decode_data([_C | T], <<-1:?int32, Rest/binary>>, IntegerDatetime, Acc) ->
    decode_data(T, Rest, IntegerDatetime, [undefined | Acc]);
decode_data([C | T], <<Len:?int32, Value:Len/binary, Rest/binary>>, IntegerDatetime, Acc) ->
    case C of
        #column{type = Type, format = 1}   -> Value2 = pgsql_binary:decode(Type, Value, IntegerDatetime);
        #column{}                          -> Value2 = Value
    end,
    decode_data(T, Rest, IntegerDatetime, [Value2 | Acc]).

%% decode column information
decode_columns(Count, Bin) ->
    decode_columns(Count, Bin, []).

decode_columns(0, _Bin, Acc) ->
    lists:reverse(Acc);
decode_columns(N, Bin, Acc) ->
    {Name, Rest} = decode_string(Bin),
    <<_Table_Oid:?int32, _Attrib_Num:?int16, Type_Oid:?int32,
     Size:?int16, Modifier:?int32, Format:?int16, Rest2/binary>> = Rest,
    Desc = #column{
      name     = Name,
      type     = pgsql_types:oid2type(Type_Oid),
      size     = Size,
      modifier = Modifier,
      format   = Format},
    decode_columns(N - 1, Rest2, [Desc | Acc]).

%% decode command complete msg
decode_complete(<<"SELECT", 0>>)   -> select;
decode_complete(<<"BEGIN", 0>>)    -> 'begin';
decode_complete(<<"ROLLBACK", 0>>) -> rollback;
decode_complete(Bin) ->
    {Str, _} = decode_string(Bin),
    case string:tokens(binary_to_list(Str), " ") of
        ["INSERT", _Oid, Rows] -> {insert, list_to_integer(Rows)};
        ["UPDATE", Rows]       -> {update, list_to_integer(Rows)};
        ["DELETE", Rows]       -> {delete, list_to_integer(Rows)};
        ["MOVE", Rows]         -> {move, list_to_integer(Rows)};
        ["FETCH", Rows]        -> {fetch, list_to_integer(Rows)};
        [Type | _Rest]         -> lower_atom(Type)
    end.

%% decode ErrorResponse
decode_error(Bin) ->
    Fields = decode_fields(Bin),
    Error = #error{
      severity = lower_atom(proplists:get_value($S, Fields)),
      code     = proplists:get_value($C, Fields),
      message  = proplists:get_value($M, Fields),
      extra    = decode_error_extra(Fields)},
    Error.

decode_error_extra(Fields) ->
    Types = [{$D, detail}, {$H, hint}, {$P, position}],
    decode_error_extra(Types, Fields, []).

decode_error_extra([], _Fields, Extra) ->
    Extra;
decode_error_extra([{Type, Name} | T], Fields, Extra) ->
    case proplists:get_value(Type, Fields) of
        undefined -> decode_error_extra(T, Fields, Extra);
        Value     -> decode_error_extra(T, Fields, [{Name, Value} | Extra])
    end.

%% encode types
encode_types(Types) ->
    encode_types(Types, 0, <<>>).

encode_types([], Count, Acc) ->
    <<Count:?int16, Acc/binary>>;

encode_types([Type | T], Count, Acc) ->
    case Type of
        undefined -> Oid = 0;
        _Any      -> Oid = pgsql_types:type2oid(Type)
    end,
    encode_types(T, Count + 1, <<Acc/binary, Oid:?int32>>).

%% encode column formats
encode_formats(Columns) ->
    encode_formats(Columns, 0, <<>>).

encode_formats([], Count, Acc) ->
    <<Count:?int16, Acc/binary>>;

encode_formats([#column{format = Format} | T], Count, Acc) ->
    encode_formats(T, Count + 1, <<Acc/binary, Format:?int16>>).

format(Type) ->
    case pgsql_binary:supports(Type) of
        true  -> 1;
        false -> 0
    end.

%% encode parameters
encode_parameters(Parameters, IntegerDatetime) ->
    encode_parameters(Parameters, 0, <<>>, <<>>, IntegerDatetime).

encode_parameters([], Count, Formats, Values, _IntegerDatetime) ->
    <<Count:?int16, Formats/binary, Count:?int16, Values/binary>>;

encode_parameters([P | T], Count, Formats, Values, IntegerDatetime) ->
    {Format, Value} = encode_parameter(P, IntegerDatetime),
    Formats2 = <<Formats/binary, Format:?int16>>,
    Values2 = <<Values/binary, Value/binary>>,
    encode_parameters(T, Count + 1, Formats2, Values2, IntegerDatetime).

%% encode parameter

encode_parameter({Type, Value}, IntegerDatetime) ->
    case pgsql_binary:encode(Type, Value, IntegerDatetime) of
        Bin when is_binary(Bin) -> {1, Bin};
        {error, unsupported}    -> encode_parameter(Value, IntegerDatetime)
    end;
encode_parameter(A, _IntegerDatetime) when is_atom(A)    -> {0, encode_list(atom_to_list(A))};
encode_parameter(B, _IntegerDatetime) when is_binary(B)  -> {0, <<(byte_size(B)):?int32, B/binary>>};
encode_parameter(I, _IntegerDatetime) when is_integer(I) -> {0, encode_list(integer_to_list(I))};
encode_parameter(F, _IntegerDatetime) when is_float(F)   -> {0, encode_list(float_to_list(F))};
encode_parameter(L, _IntegerDatetime) when is_list(L)    -> {0, encode_list(L)}.

encode_list(L) ->
    Bin = list_to_binary(L),
    <<(byte_size(Bin)):?int32, Bin/binary>>.

notify(#state{reply_to = {Pid, _Tag}}, Msg) ->
    Pid ! {pgsql, self(), Msg}.

lower_atom(Str) when is_binary(Str) ->
    lower_atom(binary_to_list(Str));
lower_atom(Str) when is_list(Str) ->
    list_to_atom(string:to_lower(Str)).

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L)   -> list_to_binary(L).

notify_async(#state{async = Pid}, Msg) ->
    case is_pid(Pid) of
        true  -> Pid ! {pgsql, self(), Msg};
        false -> false
    end.

hex(Bin) ->
    HChar = fun(N) when N < 10 -> $0 + N;
               (N) when N < 16 -> $W + N
            end,
    <<<<(HChar(H)), (HChar(L))>> || <<H:4, L:4>> <= Bin>>.

%% send data to server

send(#state{sock = Sock}, Type, Data) ->
    Bin = iolist_to_binary(Data),
    gen_tcp:send(Sock, <<Type:8, (byte_size(Bin) + 4):?int32, Bin/binary>>).

send(#state{sock = Sock}, Data) ->
    Bin = iolist_to_binary(Data),
    gen_tcp:send(Sock, <<(byte_size(Bin) + 4):?int32, Bin/binary>>).

%% -- socket read loop --

read(Fsm, Sock, Tail) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Bin} -> decode(Fsm, Sock, <<Tail/binary, Bin/binary>>);
        Error     -> exit(Error)
    end.

decode(Fsm, Sock, <<Type:8, Len:?int32, Rest/binary>> = Bin) ->
    Len2 = Len - 4,
    case Rest of
        <<Data:Len2/binary, Tail/binary>> when Type == $N ->
            gen_fsm:send_all_state_event(Fsm, {notice, decode_error(Data)}),
            decode(Fsm, Sock, Tail);
        <<Data:Len2/binary, Tail/binary>> when Type == $S ->
            [Name, Value] = decode_strings(Data),
            gen_fsm:send_all_state_event(Fsm, {parameter_status, Name, Value}),
            decode(Fsm, Sock, Tail);
        <<Data:Len2/binary, Tail/binary>> when Type == $A ->
            <<Pid:?int32, Strings/binary>> = Data,
            case decode_strings(Strings) of
                [Channel, Payload] -> ok;
                [Channel]          -> Payload = <<>>
            end,
            gen_fsm:send_all_state_event(Fsm, {notification, Channel, Pid, Payload}),
            decode(Fsm, Sock, Tail);
        <<Data:Len2/binary, Tail/binary>> ->
            gen_fsm:send_event(Fsm, {Type, Data}),
            decode(Fsm, Sock, Tail);
        _Other ->
            ?MODULE:read(Fsm, Sock, Bin)
    end;
decode(Fsm, Sock, Bin) ->
    ?MODULE:read(Fsm, Sock, Bin).
