%% @copyright Ericsson AB 1997-2022. All Rights Reserved.
%% @doc Run an os cmd with a timeout. Slightly adapted version from the
%% default Erlang os.erl in the kernel app.
%% @enddoc

%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
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
%%
%% %CopyrightEnd%

%% Copyright 2009-2022 Marc Worrell
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

-module(z_exec).

-export([
    run/1,
    run/2
]).

-include_lib("kernel/include/file.hrl").


-type os_command() :: atom() | io_lib:chars().
-type os_command_opts() :: #{
        max_size => non_neg_integer() | infinity,
        timeout => non_neg_integer()
    }.

% Default timeout for commands - 15 minutes.
-define(TIMEOUT, 900000).


%% Executes the given command in the default shell for the operating system.
-spec run(Command) -> string() when
      Command :: os_command().
run(Cmd) ->
    run(Cmd, #{ }).

-spec run(Command, Options) -> string() when
      Command :: os_command(),
      Options :: os_command_opts().
run(Cmd, Opts) ->
    try
        do_cmd(Cmd, Opts)
    catch
        throw:badopt ->
            badarg_with_cause([Cmd, Opts], badopt);
        throw:{open_port, Reason} ->
            badarg_with_cause([Cmd, Opts], {open_port, Reason});
        throw:badarg ->
            badarg_with_info([Cmd, Opts])
    end.

do_cmd(Cmd, Opts) ->
    MaxSize = get_option(max_size, Opts, infinity),
    Timeout = get_option(timeout, Opts, ?TIMEOUT),
    {SpawnCmd, SpawnOpts, SpawnInput, Eot} = mk_cmd(validate(Cmd)),
    Port = try open_port({spawn, SpawnCmd}, [binary, stderr_to_stdout,
                                             stream, in, hide | SpawnOpts])
           catch error:Reason ->
                throw({open_port, Reason})
           end,
    MonRef = erlang:monitor(port, Port),
    WatchPid = erlang:spawn(fun() -> watchdog(Port, Timeout, Cmd) end),
    true = port_command(Port, SpawnInput),
    Bytes = get_data(Port, MonRef, Eot, [], 0, MaxSize),
    demonitor(MonRef, [flush]),
    erlang:exit(WatchPid, kill),
    String = unicode:characters_to_list(Bytes),
    if  %% Convert to unicode list if possible otherwise return bytes
        is_list(String) -> String;
        true -> binary_to_list(Bytes)
    end.

get_option(Opt, Options, Default) ->
    case Options of
        #{ Opt := Value } -> Value;
        #{} -> Default;
        _ -> throw(badopt)
    end.

watchdog(Port, Timeout, Cmd) ->
    timer:sleep(Timeout),
    case erlang:port_info(Port, os_pid) of
        {os_pid, OsPid} ->
            lager:error(
                "z_exec: killing command due to timeout after ~p msec: ~s",
                [Timeout, Cmd]),
            os:cmd(io_lib:format("kill -9 ~p", [OsPid])),
            erlang:port_close(Port);
        undefined ->
            ok
    end.

mk_cmd(Cmd) ->
    %% Have to send command in like this in order to make sh commands like
    %% cd and ulimit available.
    %%
    %% We use an absolute path here because we do not want the path to be
    %% searched in case a stale NFS handle is somewhere in the path before
    %% the sh command.
    %%
    %% Check if the default shell is located in /bin/sh as expected usually
    %% or in /system/bin/sh as implemented on Android. The raw option is
    %% used to bypass the file server and speed up the file access.
    Shell = case file:read_file_info("/bin/sh",[raw]) of
                {ok,#file_info{type=regular}} ->
                    "/bin/sh";
                _ ->
                    case file:read_file_info("/system/bin/sh",[raw]) of
                        {ok,#file_info{type=regular}} ->
                            "/system/bin/sh";
                        _ ->
                            "/bin/sh"
                    end
            end,
    {Shell ++ " -s unix:cmd", [out],
     %% We insert a new line after the command, in case the command
     %% contains a comment character.
     %%
     %% The </dev/null closes stdin, which means that programs
     %% that use a closed stdin as an termination indicator works.
     %% An example of such a program is 'more'.
     %%
     %% The "echo ^D" is used to indicate that the program has executed
     %% and we should return any output we have gotten. We cannot use
     %% termination of the child or closing of stdin/stdout as then
     %% starting background jobs from os:cmd will block os:cmd.
     %%
     %% I tried changing this to be "better", but got bombarded with
     %% backwards incompatibility bug reports, so leave this as it is.
     ["(", unicode:characters_to_binary(Cmd), "\n) </dev/null; echo \"\^D\"\n"],
     <<$\^D>>}.

validate(Term) ->
    try validate1(Term)
    catch error:_ -> throw(badarg)
    end.

validate1(Atom) when is_atom(Atom) ->
    validate1(atom_to_list(Atom));
validate1(List) when is_list(List) ->
    case validate2(List) of
        false ->
            List;
        true ->
            %% Had zeros at end; remove them...
            string:trim(List, trailing, [0])
    end.

validate2([0|Rest]) ->
    validate3(Rest);
validate2([C|Rest]) when is_integer(C), C > 0 ->
    validate2(Rest);
validate2([List|Rest]) when is_list(List) ->
    validate2(List) or validate2(Rest);
validate2([]) ->
    false.

%% Ensure that the rest is zero only...
validate3([]) ->
    true;
validate3([0|Rest]) ->
    validate3(Rest);
validate3([List|Rest]) when is_list(List) ->
    validate3(List),
    validate3(Rest).


get_data(Port, MonRef, Eot, Sofar, Size, Max) ->
    receive
        {Port, {data, Bytes}} ->
            case eot(Bytes, Eot, Size, Max) of
                more ->
                    get_data(Port, MonRef, Eot, [Sofar, Bytes],
                             Size + byte_size(Bytes), Max);
                Last ->
                    catch port_close(Port),
                    flush_until_down(Port, MonRef),
                    iolist_to_binary([Sofar, Last])
            end;
        {'DOWN', MonRef, _, _, _} ->
            flush_exit(Port),
            iolist_to_binary(Sofar)
    end.

eot(Bs, <<>>, Size, Max) when Size + byte_size(Bs) < Max ->
    more;
eot(Bs, <<>>, Size, Max) ->
    binary:part(Bs, {0, Max - Size});
eot(Bs, Eot, Size, Max) ->
    case binary:match(Bs, Eot) of
        {Pos, _} when Size + Pos < Max ->
            binary:part(Bs,{0, Pos});
        _ ->
            eot(Bs, <<>>, Size, Max)
    end.

%% When port_close returns we know that all the
%% messages sent have been sent and that the
%% DOWN message is after them all.
flush_until_down(Port, MonRef) ->
    receive
        {Port, {data, _Bytes}} ->
            flush_until_down(Port, MonRef);
        {'DOWN', MonRef, _, _, _} ->
            flush_exit(Port)
    end.

%% The exit signal is always delivered before
%% the down signal, so we can be sure that if there
%% was an exit message sent, it will be in the
%% mailbox now.
flush_exit(Port) ->
    receive
        {'EXIT',  Port,  _} ->
            ok
    after 0 ->
            ok
    end.

badarg_with_cause(Args, Cause) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_kernel_errors,
                                               cause => Cause}}]).
badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_kernel_errors}}]).
