%% @author Atilla Erdodi <atilla@maximonster.com>
%% @copyright 2010 Maximonster Interactive Things
%% @doc Lightweight logging framework (for Zotonic)
%%      Several logger processes are expected to run at the same time 
%%      so fine-grained overload and flush protection can be implemented.
%%      In a basic configuration, there should be one for each WM request,
%%      one for the Zotonic core and optionally one for 'z_tracer' messages.
%%      Note, this module should be used to log lower level events.
%%      High level log messages (e.g. events by Zotonic modules) should be
%%      handled by 'mod_logging' and logged into the database.

%% Copyright 2010 Maximonster Interactive Things
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

-module(z_logger).

-behaviour(gen_server).

-export([start/1, flush_and_stop/1, drop_and_stop/1, log/3, log/4, set_loglevel/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

-record(state, {output, loglevel, log_timestamp, format, eagerness, buffer = []}).


%% 
%%
%%
%%

start(Args) ->
    Options = [],
    gen_server:start(?MODULE, Args, Options).
    
flush_and_stop(LoggerProc) ->
    gen_server:call(LoggerProc, flush_and_stop).

drop_and_stop(LoggerProc) ->
    gen_server:call(LoggerProc, drop_and_stop).

log(LoggerProc, LogLevel, TextOrBin) ->
    gen_server:cast(LoggerProc, {log, LogLevel, TextOrBin, []}).

log(LoggerProc, LogLevel, Text, Data) ->
    gen_server:cast(LoggerProc, {log, LogLevel, Text, Data}).

set_loglevel(LoggerProc, LogLevel) ->
    gen_server:cast(LoggerProc, {set_loglevel, LogLevel}).

%% --------------------

init(Args) ->
    LogLevel = proplists:get_value(loglevel, Args, 0),
    LogTimestamp = proplists:get_value(log_timestamp, Args, false),
    Format = proplists:get_value(format, Args, text),
    Eagerness = proplists:get_value(eagerness, Args, immediate),
    Output = proplists:get_value(output, Args, {file, "zotonic_"
                                                        ++ pid_to_list(self())
                                                        ++ ".log"}),
    Output2 = case Output of
                  {file, FileName} ->
                      {ok, File} = file:open(FileName, [write, raw]),
                      {file, File};
                  {udp, {Socket, Address, Port}} ->
                      {udp, {Socket, Address, Port}};
                  {udp, {Address, Port}} ->
                      {ok, Socket} = gen_udp:open(0),
                      {udp, {Socket, Address, Port}}
              end,            
    {ok, #state{loglevel=LogLevel, log_timestamp=LogTimestamp,
                format=Format, eagerness=Eagerness, output=Output2}}.
            
terminate(_Reason, _State) ->
    ok.

handle_call(flush_and_stop, _From, State) ->
    Output = State#state.output,
    Buffer = lists:reverse(State#state.buffer),
    [do_log(Output, immediate, ToLog, []) || ToLog <- Buffer],
    close(Output),
    {stop, normal, ok, State};
handle_call(drop_and_stop, _From, State) ->
    Output = State#state.output,
    close(Output),
    {stop, normal, ok, State}.    

handle_cast({log, MsgLogLevel, _Text, _Data}, State=#state{loglevel=LogLevel})
when MsgLogLevel > LogLevel ->
    {noreply, State};
handle_cast({log, _MsgLogLevel, Text, Data}, State=#state{output=Output,
                                                          log_timestamp=LogTimestamp,
                                                          format=Format, 
                                                          eagerness=Eagerness, 
                                                          buffer=Buffer}) ->    
    ToLog = fmt_log(Format, LogTimestamp, {Text, Data}),
    Buffer2 = do_log(Output, Eagerness, ToLog, Buffer),
    {noreply, State#state{buffer=Buffer2}};
handle_cast({set_loglevel, LogLevel}, State) ->
    {noreply, State#state{loglevel=LogLevel}}.

%% -----------------------------

fmt_timestamp(text) -> %TODO: specify timestamp format?
    {{Y, M, D}, {H, Mm, S}} = calendar:local_time(),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ",
                  [Y, M, D, H, Mm, S]);
fmt_timestamp(binary) ->
    Now = now(),
    term_to_binary(Now).
    
fmt_timestamp(text, Format) ->
    {{Y, M, D}, {H, Mm, S}} = calendar:local_time(),
    io_lib:format(Format,
                 [Y, M, D, H, Mm, S]).

fmt_log(binary, LogTimestamp, Binary) when is_binary(Binary) ->
    case LogTimestamp of
        true ->
            Timestamp = fmt_timestamp(binary),
            <<Timestamp/binary, Binary/binary>>;
        false ->
            Binary
    end;
fmt_log(text, LogTimestamp, {Text, Data}) ->
    case LogTimestamp of
        true ->
            Timestamp = fmt_timestamp(text),
            io_lib:format(Timestamp ++ Text, Data);
        {true, Format} ->
            Timestamp = fmt_timestamp(text, Format),
            io_lib:format(Timestamp ++ Text, Data);
        false ->            
            io_lib:format(Text, Data)
    end.

do_log({file, File}, Eagerness, ToLog, Buffer) ->
    case Eagerness of
        immediate -> 
            ok = file:write(File, ToLog),
            Buffer;
        delayed ->
            %% TODO
            ok = file:write(File, ToLog);
        at_once ->
            ok = file:write(File, ToLog),
            [ToLog | Buffer]
    end;    
do_log({udp, {Socket, Address, Port}}, _Eagerness, ToLog, Buffer) ->    
    %% TODO: buffering?
    gen_udp:send(Socket, Address, Port, ToLog),
    Buffer.
    
close({file, File}) ->
    file:close(File);
close({udp, {_Socket, _, _}}) ->
    ok. % TODO: the udp connection is shared???
