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
%%      General log messages, such as those emitted by this module
%%      should be logged by the standard error_logger (todo).

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

-export([start/1, stop/2, flush_and_stop/1, drop_and_stop/1, log/3, log/4, set_loglevel/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-record(state, {output, loglevel, log_timestamp, format, eagerness, buffer = [], wm_mref}).

%% Configuration defaults
-define(DEFAULT_LOGLEVEL, 0).
-define(DEFAULT_LOGTS, false).
-define(DEFAULT_FORMAT, text).
-define(DEFAULT_POLICY, immediate).
-define(DEFAULT_OUTPUT, {file, "zotonic_" ++ pid_to_list(self()) ++ ".log"}).

%% Timestamp format string - obviously only applicable when format == text
-define(TS_FORMATSTR, "~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B ").

%% The limit of the buffer size after it should be flushed
-define(BUFFER_SIZE_N, 8). %% TODO: shouldn't be hardcoded!

%% Overload protection settings
-define(MEM_SOFTLIMIT,  268435456). %% TODO: shouldn't be hardcoded!
-define(MEM_HARDLIMIT, 1073741824). %% TODO: shouldn't be hardcoded!
-define(MEM_SOFTL_CHKINTERVAL, 1000).
-define(MEM_HARDL_CHKINTERVAL, 5000).


%% 
%% API functions
%%

%% @doc Spawns a new logger process
start(Args) ->
    Args1 = [{wm_pid, self()} | Args],
    gen_server:start(?MODULE, Args1, []).

%% @doc Stops a logger process.
stop(LoggerProc, PolicyFunParam) ->
    gen_server:call(LoggerProc, {stop, PolicyFunParam}).

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

%%
%% gen_server callbacks
%%

init(Args) ->
    [LogLevel, LogTimestamp, Format, Eagerness, Output] =
        [proplists:get_value(Key, Args, Default) 
         || {Key, Default} <- [{loglevel, ?DEFAULT_LOGLEVEL}, 
                               {log_timestamp, ?DEFAULT_LOGTS},
                               {format, ?DEFAULT_FORMAT}, 
                               {eagerness, ?DEFAULT_POLICY},
                               {output, ?DEFAULT_OUTPUT}]],
    
    Output2 = case Eagerness of
                  {at_once, _PolicyFun} ->
                      Output; % do not open the file until it needed!
                  _ ->
                      open(Output)
              end,
              
    timer:send_interval(?MEM_SOFTL_CHKINTERVAL, check_softlimit),
    timer:send_interval(?MEM_HARDL_CHKINTERVAL, check_hardlimit),

    {wm_pid, WMPid} = proplists:lookup(wm_pid, Args),
    WMRef = erlang:monitor(process, WMPid),

    {ok, #state{loglevel=LogLevel, log_timestamp=LogTimestamp,
                format=Format, eagerness=Eagerness, output=Output2,
                wm_mref=WMRef}}.

terminate(_Reason, _State) ->
    ok.

handle_call({stop, PolicyFunParam}, _From, State=#state{eagerness=Eagerness}) ->
    WhatToDo = case Eagerness of
                   {at_once, PolicyFun} ->
                       PolicyFun(PolicyFunParam);
                   _ -> flush
               end,
    case  WhatToDo of
        flush -> do_handle_flush(State);
        drop -> drop
    end,    
    {stop, normal, ok, State};
handle_call(flush_and_stop, _From, State=#state{}) ->
    do_handle_flush(State),
    {stop, normal, ok, State};
handle_call(drop_and_stop, _From, State=#state{output=Output}) ->
    close(Output),
    {stop, normal, ok, State}.    

handle_cast({log, MsgLogLevel, _Text, _Data}, 
            State=#state{loglevel=LogLevel}) when MsgLogLevel > LogLevel ->
    {noreply, State};
handle_cast({log, _MsgLogLevel, Text, Data}, State) ->
    State2 = do_handle_log(Text, Data, State),
    {noreply, State2};
handle_cast({set_loglevel, LogLevel}, State) ->
    {noreply, State#state{loglevel=LogLevel}}.
    

%% Overload protection
handle_info(check_softlimit, State) ->
    {memory, Mem} = process_info(self(), memory),
    z_utils:flush_message(check_softlimit), 
    if 
        Mem < ?MEM_SOFTLIMIT ->
            {noreply, State};
        Mem >= ?MEM_SOFTLIMIT ->
            % it should be logged properly...
            io:format("Logger process ~p exceeded its soft memory limit. Cleaning up...\n", [self()]),
            % flush the process mailbox
            clear_mbox(),
            % force garbage collection
            % (we can't clean up the buffer yet, as the gen_server 
            %  process still holds a reference to it)
            garbage_collect(),
            % clear the buffer, hopefully it will be garbage collected soon
            {noreply, State#state{buffer=[]}}
    end;
handle_info(check_hardlimit, State=#state{output=Output}) ->
    {memory, Mem} = process_info(self(), memory),
    z_utils:flush_message(check_hardlimit), 
    if 
        Mem < ?MEM_HARDLIMIT ->
            {noreply, State};
        Mem >= ?MEM_HARDLIMIT -> %% very unlikely...
            close(Output),
            {stop, mem_hardlimit_exceeded, State}
    end;
%% TODO: check CPU/IO/etc usage

% Webmachine left without saying goodbye.
handle_info({'DOWN', Ref, process, _Pid, Reason}, State = #state{wm_mref=Ref}) ->
    State2 = do_handle_log("Unexpected Webmachine DOWN: ~p~n", [Reason], State),
    do_handle_flush(State2),
    {stop, normal, State2}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% 
%% Internal functions
%%

do_handle_log(Text, Data, 
            State=#state{output=Output,
                         log_timestamp=LogTimestamp,
                         format=Format, 
                         eagerness=Eagerness, 
                         buffer=Buffer}) ->   
    ToLog = fmt_log(Format, LogTimestamp, {Text, Data}),
    Buffer2 = do_log(Output, Eagerness, ToLog, Buffer),
    State#state{buffer=Buffer2}.

do_handle_flush(#state{output=Output, buffer=Buffer}) ->
    Output2 = open(Output),
    flush(Output2, Buffer),
    close(Output2).



fmt_timestamp(text) ->
    {{Y, M, D}, {H, Mm, S}} = calendar:local_time(),
    io_lib:format(?TS_FORMATSTR,
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


do_log(_Output, {at_once, _Fun}, ToLog, Buffer) ->
    [ToLog | Buffer];
do_log(_Output, delayed, ToLog, Buffer) when length(Buffer) < ?BUFFER_SIZE_N ->
    [ToLog | Buffer];
do_log(Output, delayed, ToLog, Buffer) ->
    flush(Output, [ToLog | Buffer]),
    [];
do_log({file, {io_device, File}}, immediate, ToLog, Buffer) ->
    ok = file:write(File, ToLog),
    Buffer;
do_log({udp, {Socket, Address, Port}}, immediate, ToLog, Buffer) ->
    gen_udp:send(Socket, Address, Port, ToLog),
    Buffer.



open(Output) ->
    case Output of
        {file, {io_device, File}} ->
            {file, {io_device, File}};
        {file, FileName} ->
            {ok, File} = file:open(FileName, [write, raw]),
            {file, {io_device, File}};
        {udp, {Socket, Address, Port}} ->
            {udp, {Socket, Address, Port}};
        {udp, {Address, Port}} ->
            {ok, Socket} = gen_udp:open(0),
            {udp, {Socket, Address, Port}}
    end.

flush(Output, Buffer) ->
    Buffer2 = lists:reverse(Buffer),
    [do_log(Output, immediate, ToLog, []) || ToLog <- Buffer2].
    
close({file, {io_device, File}}) ->
    file:close(File);
close({udp, {_Socket, _, _}}) ->
    ok; % TODO: the udp connection is shared???
close(_) ->
    ok.

%%
%% Overload protection - Internal functions
%%

clear_mbox() ->
    receive _ -> clear_mbox() after 0 -> ok end.

overload_test() ->
    MessagesAtOnce = 50000,
    N = 10000,
    {ok, P} = z_logger:start([{eagerness, {at_once, fun(_) -> drop end}}]),
    [begin
         [z_logger:log(P, 0, "text ~p ~p ~n",
                [atom, I]) || I <- lists:seq(1, MessagesAtOnce)],
            
         io:format("[~p bytes]", [element(2, process_info(P, memory))])
     end
     || _ <- lists:seq(1, N)].


