%%
%%
%%

-module(z_stats).

-include_lib("zotonic.hrl").
-include_lib("webmachine_logger.hrl").

-export([
    init/0,
    new/1, new/2,
    update/2,
    timed_update/3, timed_update/4, timed_update/5]).

%% Act as a webmachine logger.
-export([log_access/1]).

%% @doc Initialize the statistics collection machinery.
%%
init() ->
    z_stat_handler:init().

%% @doc Create a new counter, or histogram for zotonic
%% 
new(Stat) ->
    new(Stat, #stats_from{}).

%% @doc Create a new counter or histogram.
%%
new(Stat, StatsFrom) ->
    z_stat_handler:new(Stat, StatsFrom).


%% @doc Update a counter, histogram, whatever.
%%
update(What, StatsFrom) when is_tuple(StatsFrom) ->
    z_stat_handler:update(What, StatsFrom);
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
log_access(#wm_log_data{start_time=undefined}) -> 
    ok;
log_access(#wm_log_data{end_time=undefined}) -> 
    ok;
log_access(#wm_log_data{start_time=StartTime, end_time=EndTime, response_length=ResponseLength}=LogData) ->
    try 
        Duration = #histogram{name=duration, value=timer:now_diff(EndTime, StartTime)},
        Out = #counter{name=out, value=ResponseLength},
        System = #stats_from{system=webzmachine},

        %% The request has already been counted by z_sites_dispatcher.
        For = case webmachine_logger:get_metadata(zotonic_host, LogData) of
            undefined -> System;
            Host -> [System, System#stats_from{host=Host}]
        end,

        update(Duration, For),
        update(Out, For)
    after 
        % Pass it to the default webmachine logger.
        webmachine_logger:log_access(LogData)
    end.
