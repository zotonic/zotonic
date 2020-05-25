%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020 Marc Worrell
%% @doc Run a pivot task queue job.

%% Copyright 2020 Marc Worrell
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

-module(z_pivot_rsc_task_job).

-export([
    start_task/3,

    task_job/3
    ]).

-include_lib("zotonic.hrl").

% Max number of times a task will retry on fatal exceptions
-define(MAX_TASK_ERROR_COUNT, 5).


%% @doc Start a task queue sidejob.
-spec start_task( pid(), map(), z:context() ) -> {ok, pid()} | {error, overload}.
start_task(PivotPid, Task, Context) ->
    sidejob_supervisor:spawn(
            zotonic_sidejobs,
            {?MODULE, task_job, [ PivotPid, Task, Context ]}).

%% @doc Run the sidejob task queue task.
-spec task_job( pid(), map(), z:context() ) -> ok.
task_job(
    PivotPid,
    #{
        task_id := TaskId,
        mfa := {Module, Function, Args},
        error_count := ErrCt
    }, Context) ->
    try
        lager:debug("Pivot task starting: ~p:~p(...)", [ Module, Function ]),
        case call_function(Module, Function, ensure_list(Args), Context) of
            {delay, Delay} ->
                Due = if
                        is_integer(Delay) ->
                            calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Delay);
                        is_tuple(Delay) ->
                            Delay
                      end,
                z_db:update(pivot_task_queue, TaskId, [ {due, Due} ], Context);
            {delay, Delay, NewArgs} ->
                Due = if
                        is_integer(Delay) ->
                            calendar:gregorian_seconds_to_datetime(
                                calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Delay);
                        is_tuple(Delay) ->
                            Delay
                      end,
                Fields = #{
                    <<"due">> => Due,
                    <<"args">> => NewArgs
                },
                z_db:update(pivot_task_queue, TaskId, Fields, Context);
            _OK ->
                z_db:delete(pivot_task_queue, TaskId, Context)
        end
    catch
        ?WITH_STACKTRACE(error, undef, Trace)
            lager:error("Task ~p failed - undefined function, aborting: ~p:~p(~p) ~p",
                        [TaskId, Module, Function, Args, Trace]),
            z_db:delete(pivot_task_queue, TaskId, Context);
        ?WITH_STACKTRACE(Error, Reason, Trace)
            case ErrCt < ?MAX_TASK_ERROR_COUNT of
                true ->
                    RetryDue = calendar:gregorian_seconds_to_datetime(
                            calendar:datetime_to_gregorian_seconds(calendar:universal_time())
                            + task_retry_backoff(ErrCt)),
                    lager:error("Task ~p failed - will retry ~p:~p(~p) ~p:~p on ~p ~p",
                                [TaskId, Module, Function, Args, Error, Reason, RetryDue, Trace]),
                    RetryFields = #{
                        <<"due">> => RetryDue,
                        <<"error_ct">> => ErrCt+1
                    },
                    z_db:update(pivot_task_queue, TaskId, RetryFields, Context);
                false ->
                    lager:error("Task ~p failed - aborting ~p:~p(~p) ~p:~p ~p",
                                [TaskId, Module, Function, Args, Error, Reason, Trace]),
                    z_db:delete(pivot_task_queue, TaskId, Context)
            end
    after
        ok = gen_server:call(PivotPid, {task_done, TaskId, self()}, infinity)
    end.


call_function(Module, Function, As, Context) ->
    code:ensure_loaded(Module),
    AsLen = length(As),
    case erlang:function_exported(Module, Function, AsLen+1) of
        true ->
            % Assume function with extra Context arg
            erlang:apply(Module, Function, As ++ [Context]);
        false ->
            % Function called with only the arguments list
            erlang:apply(Module, Function, As)
    end.

task_retry_backoff(0) -> 10;
task_retry_backoff(1) -> 1800;
task_retry_backoff(2) -> 7200;
task_retry_backoff(3) -> 14400;
task_retry_backoff(4) -> 12 * 3600;
task_retry_backoff(N) -> (N-4) * 24 * 3600.

ensure_list(L) when is_list(L) -> L;
ensure_list(undefined) -> [];
ensure_list(X) -> [X].
