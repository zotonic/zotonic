%% Collection of statistic operations.

-record(stats_from, 
    {host=zotonic,
     system=core
    }).

-record(counter, {name, op=inc, value=1}).

-record(histogram, {name, value=undefined}).

-record(gauge, {name, value=undefined}).

