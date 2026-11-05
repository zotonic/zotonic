%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Monotonic operation budgets, including hashing, queueing and transfers.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(z_media_deadline).
-export([remaining/2, run/2]).

-spec remaining(map(), pos_integer()) -> non_neg_integer().
remaining(Options, Limit) ->
    case maps:find(media_runner_deadline, Options) of
        {ok, Deadline} -> max(0, min(Limit, Deadline - erlang:monotonic_time(millisecond)));
        error -> Limit
    end.

%% The watchdog also terminates work if its caller disappears. Use a separate
%% monitor process so a busy worker cannot postpone handling its owner's DOWN.
-spec run(fun(() -> term()), integer()) -> term().
run(Fun, Deadline) ->
    Parent = self(),
    Tag = make_ref(),
    {Pid, Ref} = spawn_monitor(fun() ->
        receive {start, Guard} ->
            Result = Fun(),
            Parent ! {Tag, Result},
            Guard ! done
        end
    end),
    Guard = spawn(fun() ->
        M = monitor(process, Parent),
        W = monitor(process, Pid),
        receive
            done -> ok;
            {'DOWN', W, process, Pid, _} -> ok;
            {'DOWN', M, process, Parent, _} -> exit(Pid, kill)
        after max(0, Deadline - erlang:monotonic_time(millisecond)) -> exit(Pid, kill)
        end
    end),
    Pid ! {start, Guard},
    receive
        {Tag, Result} -> demonitor(Ref, [flush]), Result;
        {'DOWN', Ref, process, Pid, killed} -> {error, {media_runner_unavailable, operation_timeout}};
        {'DOWN', Ref, process, Pid, Reason} -> {error, {media_worker, Reason}}
    end.
