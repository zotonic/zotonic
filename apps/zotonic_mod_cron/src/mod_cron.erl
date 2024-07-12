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

-record(state, { site :: atom() }).

%% Default timers. These are called approximately every N seconds.
-define(TIMER_INTERVAL, [
    {    1, tick_1s},
    {   60, tick_1m},
    {  600, tick_10m},
    {  900, tick_15m},
    { 1800, tick_30m},
    { 3600, tick_1h},
    { 7200, tick_2h},
    {10800, tick_3h},
    {14400, tick_4h},
    {21600, tick_6h},
    {43200, tick_12h},
    {86400, tick_24h}
]).


start_link(Args) when is_list(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Site = z_context:site(Context),
    gen_server:start_link(?MODULE, Site, []).

init(Site) when is_atom(Site) ->
    z_context:logger_md(Site),
    lists:foreach(
        fun({Timeout, Event}) ->
            erlang:send_after(Timeout * 1000, self(), {tick, Timeout, Event})
        end,
        ?TIMER_INTERVAL),
    {ok, #state{ site = Site }}.

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_message}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tick, Timeout, Event}, #state{ site = Site } = State) ->
    z_notifier:notify(Event, z_context:new(Site)),
    erlang:send_after(Timeout * 1000, self(), {tick, Timeout, Event}),
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
