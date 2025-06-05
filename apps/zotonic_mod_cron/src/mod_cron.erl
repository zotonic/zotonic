%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2024 Marc Worrell
%% @doc Periodic tasks and ticks for other modules.
%% @end

%% Copyright 2017-2024 Marc Worrell
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
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Cron").
-mod_description("Periodic tasks and ticks for other modules.").
-mod_provides([cron]).
-mod_prio(500).

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

-export([ tick_event/2 ]).

-record(state, {
          site :: atom(),
          tick_job_refs = [] :: list(erlcron:job_ref())
         }).

%% Default timers. These are called approximately every N seconds.
-define(TIMER_INTERVAL, [
    {{ 1, sec}, tick_1s},
    {{ 1, min}, tick_1m},
    {{10, min}, tick_10m},
    {{15, min}, tick_15m},
    {{30, min}, tick_30m},
    {{ 1, hr},  tick_1h},
    {{ 2, hr},  tick_2h},
    {{ 3, hr},  tick_3h},
    {{ 4, hr},  tick_4h},
    {{ 6, hr},  tick_6h},
    {{12, hr},  tick_12h},
    {{24, hr},  tick_24h}
]).

-include_lib("zotonic_core/include/zotonic.hrl").

tick_event(Event, Site) ->
    z_notifier:notify(Event, z_context:new(Site)).

start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Site = z_context:site(Context),
    gen_server:start_link(?MODULE, Site, []).

init(Site) when is_atom(Site) ->
    erlang:process_flag(trap_exit, true),
    z_context:logger_md(Site),
    JobRefs = start_tick_jobs(Site),
    {ok, #state{ site = Site, tick_job_refs = JobRefs }}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{ tick_job_refs = TickJobs }) ->
    lists:foreach(fun(JobRef) -> _ = erlcron:cancel(JobRef) end, TickJobs),
    ok.

%%
%% Helpers
%%

start_tick_jobs(Site) ->
    [ start_tick_job(Event, Timeout, Site) || {Timeout, Event} <- ?TIMER_INTERVAL].

start_tick_job(Event, Timeout, Site) ->
    JobRef = job_ref(Event, Site),
    Job = #{ id => JobRef,
             interval => {daily, {every, Timeout}},
             execute => {?MODULE, tick_event, [Event, Site]} },
    case erlcron:cron(Job) of
        already_started -> JobRef;
        JobRef -> JobRef
    end.

job_ref(Event, Site) ->
    <<(z_convert:to_binary(Site))/binary, $_, (z_convert:to_binary(Event))/binary>>.
