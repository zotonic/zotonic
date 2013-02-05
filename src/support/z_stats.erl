%%
%%
%%

-module(z_stats).

-include_lib("zotonic.hrl").

-export([
    init/0, 
    update/1, update/2,
    timed_update/3, timed_update/4, timed_update/5]).

%% @doc Initialize the statistics collection machinery.
%%
init() ->
    ?DEBUG(init),
    catch z_stat_handler:init().

%% @doc Update a counter, histogram, whatever.
%%
update(What) ->
    update(What, #stats_from{}).

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

update(What, StatsFrom) ->
    %%?DEBUG({What, StatsFrom}),
    catch z_stat_handler:update(What, StatsFrom).
