%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2023 Marc Worrell
%% @doc Execute OS commands with timeouts.
%% @end

%% Copyright 2023 Marc Worrell
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

-type os_command() :: iodata().
-type os_command_opts() :: #{
        max_size => non_neg_integer() | infinity,
        timeout => non_neg_integer()
    }.

% Default timeout for commands - 15 minutes.
-define(TIMEOUT, 900000).

% Default maximum stdout size for commands - 1GB
-define(MAX_SIZE, 1073741824).

% Timeout to kill a command after stop has been signaled
-define(KILL_TIMEOUT_SECS, 10).

%% @doc Executes the given command in the default shell for the operating system. Run with
%% a default timeout of 15 minutes and a default maximum returned size of 1GB.
-spec run(Command) -> {ok, Data} | {error, Reason} when
      Command :: os_command(),
      Data :: binary(),
      Reason :: term().
run(Command) ->
    run(Command, #{}).


%% @doc Executes the given command in the default shell for the operating system.
%% The option max_data will terminate the program after more data than the given number
%% of bytes is received. Use 'infinity' to not restrict the output size.
%% The option timeout will terminate the program if more time than the given milliseconds
%% have passed, in that case {error, timeout} is returned.
-spec run(Command, Options) -> {ok, Data} | {error, Reason} when
      Command :: os_command(),
      Options :: os_command_opts(),
      Data :: binary(),
      Reason :: term().
run(Commmand, Options) ->
    Timeout = maps:get(timeout, Options, ?TIMEOUT),
    MaxSize = maps:get(max_size, Options, ?MAX_SIZE),
    ExecOptions = [
        stdout,
        monitor,
        {kill_timeout, ?KILL_TIMEOUT_SECS}
    ],
    case exec:run(Commmand, ExecOptions) of
        {ok, _Pid, OsPid} ->
            {ok, Timer} = timer:send_after(Timeout, {timeout, OsPid}),
            Result = receive_data(OsPid, MaxSize, <<>>),
            % Cancel timeout timer and clear optional late timeout message
            timer:cancel(Timer),
            receive
                {timeout, OsPid} ->
                    ok
                after 0 ->
                    ok
            end,
            Result;
        {error, _} = Error ->
            Error
    end.

receive_data(OsPid, MaxSize, Acc) when MaxSize =:= infinity orelse size(Acc) =< MaxSize ->
    receive
        {'DOWN', OsPid, process, _, normal} ->
            {ok, Acc};
        {'DOWN', OsPid, process, _, Reason} ->
            {error, Reason};
        {timeout, OsPid} ->
            exec:stop(OsPid),
            receive
                {'DOWN', OsPid, process, _, _Reason} ->
                    {error, timeout}
            end;
        {stdout, OsPid, Data} ->
            receive_data(OsPid, MaxSize, <<Acc/binary, Data/binary>>)
    end;
receive_data(OsPid, _MaxSize, Acc) ->
    exec:stop(OsPid),
    receive
        {'DOWN', OsPid, process, _, _Reason} ->
            {ok, Acc}
    end.
