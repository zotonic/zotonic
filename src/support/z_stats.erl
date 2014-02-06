%% @author Maas-Maarten Zeeman <mmzeema@xs4all.nl>
%% @copyright 2013 Maas-Maarten Zeeman
%% Date: 2013-02-17
%% @doc Server for matching the request path to correct site and dispatch rule.

%% Copyright 2013 Maas-Maarten Zeeman
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

-module(z_stats).

-include_lib("zotonic.hrl").
-include_lib("webzmachine/include/webmachine_logger.hrl").

-export([
    init/0,
    new/2,
    update/2,
    timed_update/3, timed_update/4, timed_update/5]).

%% Act as a webmachine logger.
-export([log_access/1]).


%% @doc Initialize the statistics collection machinery.
%%
init() ->
    folsom:start(),
    webmachine_sup:start_logger(webmachine_logger).

%% @doc Create a new counters and histograms.
%%
new(Stat, From) ->
    Key = key(Stat, From),
    case Stat of
        #counter{} ->
            folsom_metrics:new_meter(Key);
        #histogram{} ->
            folsom_metrics:new_histogram(Key, slide)
    end,
    folsom_metrics:tag_metric(Key, tag(From)).

%% @doc Update a counter, histogram, whatever.
%%
update(What, StatsFrom) when is_tuple(StatsFrom) ->
    update_metric(What, StatsFrom);
update(_Stat, []) ->
    ok;
update(Stat, [H|T]) ->
    update(Stat, H),
    update(Stat, T).

%% @doc Execute the function, and store the measured execution time.
%%
timed_update(Name, Fun, StatsFrom) ->
    {Time, Result} = timer:tc(Fun),
    update(#histogram{name=Name, value=Time}, StatsFrom),
    Result.

timed_update(Name, Fun, Args, StatsFrom) ->
    {Time, Result} = timer:tc(Fun, Args),
    update(#histogram{name=Name, value=Time}, StatsFrom),
    Result.

timed_update(Name, Mod, Fun, Args, StatsFrom) ->
    {Time, Result} = timer:tc(Mod, Fun, Args),
    update(#histogram{name=Name, value=Time}, StatsFrom),
    Result.


%% @doc Collect log data from webzmachine.
%%
log_access(#wm_log_data{finish_time=undefined}=LogData) -> 
    log_access(LogData#wm_log_data{finish_time=os:timestamp()});
log_access(#wm_log_data{start_time=StartTime, finish_time=FinishTime, 
                        response_length=ResponseLength}=LogData) when StartTime =/= undefined ->
    try 
        Duration = #histogram{name=duration, value=timer:now_diff(FinishTime, StartTime)},
        Out = #counter{name=out, value=ResponseLength},
        System = #stats_from{system=webzmachine},

        %% The request has already been counted by z_sites_dispatcher.
        For = case webmachine_logger:get_metadata(zotonic_host, LogData) of
            undefined -> 
                System;
            Host -> 
                [System, System#stats_from{host=Host}]
        end,

        update(Duration, For),
        update(Out, For)
    after 
        % Pass it to the default webmachine logger.
        webmachine_logger:log_access(LogData)
    end.

    
%% Some helper functions.

update_metric(#counter{op=incr, value=Value}=Stat, From) ->
    folsom_metrics:safely_notify(key(Stat, From), Value);
update_metric(#histogram{value=Value}=Stat, From) ->
    folsom_metrics:safely_notify({key(Stat, From), Value}).
  
key(#counter{name=Name}, #stats_from{host=Host, system=System}) ->
	{Host, System, Name};
key(#histogram{name=Name}, #stats_from{host=Host, system=System}) ->
	{Host, System, Name}.    

tag(#stats_from{host=Host}) ->
    Host.
