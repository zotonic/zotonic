%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2026 Marc Worrell
%% @doc Periodic tasks and ticks for other modules.
%% @end

%% Copyright 2017-2026 Marc Worrell
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

-module(mod_cron).
-moduledoc("
Provides periodic tick notifications and module-defined cron job scheduling.

This module both sends periodic *tick* notifications (which can be observed by modules) and reads each module’s
`-mod_cron_jobs` attribute to register scheduled jobs at specific intervals.

tick
----

For periodic tasks the system has various periodic tick events. They are named after their interval *s* for seconds, *m*
for minutes, and *h* for hours.

*   tick_1s
*   tick_1m
*   tick_10m
*   tick_15m
*   tick_30m
*   tick_1h
*   tick_2h
*   tick_3h
*   tick_4h
*   tick_6h
*   tick_12h
*   tick_24h



### Example

Check something every hour.


```erlang
-include_lib(\"kernel/include/logger.hrl\").

observe_tick_1h(tick_1h, Context) ->
    ?LOG_INFO(\"And another hour has passed...\"),
    do_something(Context).
```

The return value is ignored.

The *tick* observers are called one by one in a separate process. So a slow handler can delay the other handlers.



`-mod_cron_jobs` Module Attribute
---------------------------------

Modules can define periodic jobs using the module attribute `-mod_cron_jobs`.

`mod_cron` reads this attribute from active modules and registers each job with `erlcron`.

Each entry is one of:

* `{RunWhen, {Module, Function, Args}}`
* `{RunWhen, {Module, Function, Args}, JobOpts}`

Where:

* `RunWhen` is an `erlcron` schedule tuple (daily/weekly/every/etc.).
* `{Module, Function, Args}` is the MFA to call.
* `JobOpts` are optional `erlcron` job options.

Important: `mod_cron` appends the site `Context` as the last argument when calling the MFA, so the target function must
accept one extra argument.

### Example

```erlang
-mod_depends([cron]).
-mod_cron_jobs([
    {{weekly, mon, {10, 0, 0}}, {?MODULE, send_weekly_reminders, []}}
]).

send_weekly_reminders(Context) ->
    Events = get_this_weeks_events(Context),
    send_email_reminders(Events, Context).
```

This schedules `send_weekly_reminders/1` every Monday at 10:00 UTC.

Scheduling syntax is provided by `erlcron`, see:
<https://hexdocs.pm/erlcron/readme.html>

Accepted Events
---------------

This module handles the following notifier callbacks:

- `observe_tick_1h`: Run periodic hourly cron checks and enqueue due jobs.

").
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Cron").
-mod_description("Periodic tasks and ticks for other modules.").
-mod_provides([cron]).
-mod_prio(500).
-mod_cron_jobs([
    {{daily, {every, { 1, sec}}}, {z_notifier, notify, [tick_1s]}},
    {{daily, {every, { 1, min}}}, {z_notifier, notify, [tick_1m]}},
    {{daily, {every, { 5, min}}}, {z_notifier, notify, [tick_5m]}},
    {{daily, {every, {10, min}}}, {z_notifier, notify, [tick_10m]}},
    {{daily, {every, {15, min}}}, {z_notifier, notify, [tick_15m]}},
    {{daily, {every, {30, min}}}, {z_notifier, notify, [tick_30m]}},
    {{daily, {every, { 1, hr}}},  {z_notifier, notify, [tick_1h]}},
    {{daily, {every, { 2, hr}}},  {z_notifier, notify, [tick_2h]}},
    {{daily, {every, { 3, hr}}},  {z_notifier, notify, [tick_3h]}},
    {{daily, {every, { 4, hr}}},  {z_notifier, notify, [tick_4h]}},
    {{daily, {every, { 6, hr}}},  {z_notifier, notify, [tick_6h]}},
    {{daily, {every, {12, hr}}},  {z_notifier, notify, [tick_12h]}},
    {{daily, {every, {24, hr}}},  {z_notifier, notify, [tick_24h]}}
]).

-behaviour(gen_server).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {
          jobs = #{},
          context :: z:context()
         }).

-include_lib("zotonic_core/include/zotonic.hrl").

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    erlang:process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),

    Site = z_context:site(Context),
    logger:set_process_metadata(#{
        site => Site,
        module => ?MODULE
    }),

    z_notifier:observe(module_activate, self(), Context),
    z_notifier:observe(module_deactivate, self(), Context),

    State = add_jobs(?MODULE, #state{context = Context}),
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_cast({#module_activate{ module = Module }, _Context}, State) ->
    State1 = add_jobs(Module, State),
    {noreply, State1};
handle_cast({#module_deactivate{ module = Module }, _Context}, State) ->
    State1 = cancel_jobs(Module, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{ context = Context, jobs = Jobs}) ->
    z_notifier:detach(module_activate, self(), Context),
    z_notifier:detach(module_deactivate, self(), Context),
    maps:foreach(fun(_Module, ModuleJobs) -> _ = cancel_jobs(ModuleJobs) end, Jobs),
    ok.

%%
%% Helpers
%%

jobs(Module) ->
    try
        Info = erlang:get_module_info(Module, attributes),
        lists:flatten(proplists:get_all_values(mod_cron_jobs, Info))
    catch
        error:badarg -> []
    end.

add_jobs(Module, State) ->
    %% Cancel exsisting jobs
    State1 = cancel_jobs(Module, State),

    %% Add jobs defined in the module
    ModuleJobs = jobs(Module),
    case add_jobs1(ModuleJobs, State1#state.context) of
        [] ->
            State;
        ModuleJobRefs ->
            Jobs = State1#state.jobs,
            State1#state{ jobs = Jobs#{ Module => ModuleJobRefs }}
    end.
add_jobs1(Jobs, Context) ->
    JobRefs = [add_job(Job, Context) || Job <- Jobs],
    [Job || Job <- JobRefs, Job =/= undefined].

add_job({RunWhen, MFA}, Context) ->
    add_job({RunWhen, MFA, #{}}, Context);
add_job({RunWhen, {M, F, A}, JobOpts}=Clause, Context) ->
    try
        case erlcron:cron( {RunWhen, {M, F, A ++ [Context]}, JobOpts}) of
            ignored ->
                ?LOG_ERROR(#{ text => <<"Could not add job">>,
                              reason => ignored,
                              job => Clause}
                          ),
                undefined;
            {error, Reason} ->
                ?LOG_ERROR(#{ text => <<"Could not add job">>,
                              reason => {error, Reason},
                              job => Clause}),
                undefined;
            Ref when is_atom(Ref) orelse is_reference(Ref) orelse is_binary(Ref) ->
                Ref
        end
    catch
        %% erlcron throws an exception when the arity of the function
        %% is wrong or if the module does not exist. Catching it here
        %% prevents mod_cron for the entire site to be stopped if one
        %% module has a misconfigured job.
        error:ErrorReason ->
            ?LOG_ERROR(#{ text => <<"Could not add job">>,
                          error => {catched, {error, ErrorReason}},
                          job => Clause
                        }),
            undefined
    end;
add_job(Clause, _Context) ->
    ?LOG_ERROR(#{ text => <<"Could not add job">>,
                  reason => wrong_job_clause,
                  job => Clause }),
    undefined.

% Cancel all jobs defined by a module
cancel_jobs(Module, #state{ jobs = Jobs }=State) ->
    cancel_jobs(maps:get(Module, Jobs, [])),
    State#state{ jobs = maps:without([Module], Jobs) }.

cancel_jobs(Jobs) ->
    lists:foreach( fun(Job) -> _ = erlcron:cancel(Job) end, Jobs).
