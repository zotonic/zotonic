%% Collection of statistic operations.

-record(stats_from, 
    {host=zotonic,
     system=core
    }).

-record(counter, {name, op=incr, value=1}).

-record(histogram, {name, value=undefined}).

