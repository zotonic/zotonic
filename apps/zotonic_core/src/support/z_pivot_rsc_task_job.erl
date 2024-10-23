%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2020-2024 Marc Worrell
%% @doc Run a pivot task queue job.
%% @end

%% Copyright 2020-2024 Marc Worrell
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
    start_task/2,

    task_job/2,

    maybe_schedule_retry/5,
    task_retry_due/1
    ]).

-include_lib("zotonic.hrl").

% Max number of times a task will retry on fatal exceptions
-define(MAX_TASK_ERROR_COUNT, 10).


%% @doc Start a task queue sidejob.
-spec start_task( map(), z:context() ) -> {ok, pid()} | {error, overload}.
start_task(Task, Context) ->
    z_sidejob:start(?MODULE, task_job, [ Task ], Context).

%% @doc Run the sidejob task queue task.
-spec task_job( map(), z:context() ) -> ok.
task_job(
    #{
        task_id := TaskId,
        mfa := {Module, Function, Args}
    } = Task, Context) ->
    z_context:logger_md(Context),
    try
        Args1 = ensure_list(Args),
        ?LOG_DEBUG(#{
            text => <<"Pivot task starting">>,
            in => zotonic_core,
            mfa => {Module, Function, length(Args1)+1}
        }),
        case call_function(Module, Function, Args1, Context) of
            {delay, Delay} ->
                Due = delay_to_after(Delay),
                z_db:update(pivot_task_queue, TaskId, [ {due, Due} ], Context),
                z_pivot_rsc:publish_task_event(delay, Module, Function, Due, Context);
            {delay, Delay, NewArgs} ->
                Due = delay_to_after(Delay),
                Fields = #{
                    <<"due">> => Due,
                    <<"args">> => NewArgs
                },
                z_db:update(pivot_task_queue, TaskId, Fields, Context),
                z_pivot_rsc:publish_task_event(delay, Module, Function, Due, Context);
            _OK ->
                z_db:delete(pivot_task_queue, TaskId, Context),
                z_pivot_rsc:publish_task_event(delete, Module, Function, undefined, Context)
        end
    catch
        error:undef:Trace ->
            ?LOG_ERROR(#{
                text => <<"Pivot task failed - undefined function, aborting">>,
                in => zotonic_core,
                task_id => TaskId,
                mfa => {Module, Function, Args},
                result => error,
                reason => undef,
                stack => Trace
            }),
            z_db:delete(pivot_task_queue, TaskId, Context),
            z_pivot_rsc:publish_task_event(delete, Module, Function, undefined, Context);
        Error:Reason:Trace ->
            maybe_schedule_retry(Task, Error, Reason, Trace, Context)
    after
        z_pivot_rsc:task_job_done(TaskId, Context)
    end,
    ok.

delay_to_after(undefined) -> undefined;
delay_to_after(0) -> undefined;
delay_to_after(Delay) when is_integer(Delay) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(calendar:universal_time()) + Delay);
delay_to_after(Due) when is_tuple(Due) ->
    Due.

-spec maybe_schedule_retry(map(), atom(), term(), list(), z:context()) -> ok.
maybe_schedule_retry(#{ task_id := TaskId, error_count := ErrCt, mfa := MFA }, Error, Reason, Trace, Context) 
    when ErrCt < ?MAX_TASK_ERROR_COUNT ->
    RetryDue = task_retry_due(ErrCt),
    ?LOG_ERROR(#{
        text => <<"Pivot task failed - will retry">>,
        in => zotonic_core,
        task_id => TaskId,
        mfa => MFA,
        result => Error,
        reason => Reason,
        retry_on => RetryDue,
        stack => Trace
    }),
    RetryFields = #{
        <<"due">> => RetryDue,
        <<"error_count">> => ErrCt+1
    },
    {ok, _} = z_db:update(pivot_task_queue, TaskId, RetryFields, Context),
    {Module, Function, _} = MFA,
    z_pivot_rsc:publish_task_event(retry, Module, Function, RetryDue, Context),
    ok;
maybe_schedule_retry(#{ task_id := TaskId, mfa := MFA }, Error, Reason, Trace, Context) ->
    ?LOG_ERROR(#{
        text => <<"Pivot task failed - aborting">>,
        in => zotonic_core,
        task_id => TaskId,
        mfa => MFA,
        result => Error,
        reason => Reason,
        stack => Trace
    }),
    z_db:delete(pivot_task_queue, TaskId, Context),
    {Module, Function, _} = MFA,
    z_pivot_rsc:publish_task_event(delete, Module, Function, undefined, Context),
    {error, stopped}.

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

-spec task_retry_due( integer() ) -> calendar:datetime().
task_retry_due(ErrCt) ->
    calendar:gregorian_seconds_to_datetime(
            calendar:datetime_to_gregorian_seconds(calendar:universal_time())
            + task_retry_backoff(ErrCt)).

task_retry_backoff(0) -> 10;
task_retry_backoff(1) -> 1800;
task_retry_backoff(2) -> 7200;
task_retry_backoff(3) -> 14400;
task_retry_backoff(4) -> 12 * 3600;
task_retry_backoff(N) -> (N-4) * 24 * 3600.

ensure_list(L) when is_list(L) -> L;
ensure_list(undefined) -> [];
ensure_list(X) -> [X].
