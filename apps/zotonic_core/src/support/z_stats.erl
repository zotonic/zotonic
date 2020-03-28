%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013-2015 Maas-Maarten Zeeman
%% Date: 2015-11-16
%% @doc Module for handling request statistics.

%% Copyright 2013-2015 Maas-Maarten Zeeman
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

-export([init/0, init_site/1]).

%% Act as a webmachine logger
-export([log_access/1]).


%% @doc Initialize the statistics collection machinery.
%%
init() ->
    create_vm_metrics(),
    setup_mqtt_reporter(),



    ok.

% @doc Setup stats for each site.
init_site(Host) ->
    %% Webzmachine metrics
    %%
    %% [TODO] make aggregated  of this.
    exometer:re_register([zotonic, Host, cowmachine, requests], counter, []),
    exometer:re_register([zotonic, Host, cowmachine, duration], histogram, []),
    exometer:re_register([zotonic, Host, cowmachine, data_out], counter, []),

    exometer:re_register([zotonic, Host, depcache, evictions], counter, []),

    %% Database metrics
    exometer:re_register([zotonic, Host, db, requests], counter, []),
    exometer:re_register([zotonic, Host, db, duration], histogram, []),

    %% Session metrics
    %% [TODO] add mqtt sessions
    %% exometer:re_register([zotonic, Host, session, sessions], gauge, []),
    %% exometer:re_register([zotonic, Host, session, page_processes], counter, []),

    ok.


%% @doc Collect log data from cowmachine and update cowmachine metrics
%%

log_access(MetricsData) ->
    try
        handle_stats(MetricsData)
    after
        z_access_syslog:log_access(MetricsData)
    end.


handle_stats(MetricsData) ->
    Site = get_site(MetricsData),
    DispatchRule = get_dispatch_rule(MetricsData),
    count_request(Site, DispatchRule),
    measure_duration(Site, DispatchRule,
                     duration(maps:get(req_start, MetricsData, undefined),
                              maps:get(resp_end, MetricsData, undefined))),
    measure_data_out(Site, DispatchRule,
                     maps:get(resp_body_length, MetricsData)).


count_request(Site, DispatchRule) when is_atom(Site)
                                       andalso is_atom(DispatchRule) ->
    ok = exometer:update_or_create([zotonic, Site, cowmachine, DispatchRule, requests], 1, spiral, []);
count_request(_Site, _DispatchRule) ->
    ok.

measure_duration(Site, DispatchRule, Duration) when is_atom(Site)
                                                    andalso is_atom(DispatchRule)
                                                    andalso is_integer(Duration) ->
    ok = exometer:update_or_create([zotonic, Site, cowmachine, DispatchRule, duration], Duration, histogram, []);
measure_duration(_Site, _Dispatch, _Duration) ->
    ok.

measure_data_out(Site, Dispatch, DataOut) when is_atom(Site)
                                               andalso is_atom(Dispatch)
                                               andalso is_integer(DataOut) ->
    ok = exometer:update_or_create([zotonic, Site, cowmachine, Dispatch, data_out], DataOut, spiral, []);
measure_data_out(_Site, _Dispatch, _DataOut) ->
    ok.


%%
%% Helpers
%%

% @private Return the site name from the user-data
get_site(#{user_data := #{site := Site}}) -> Site;
get_site(#{}) -> '$unknown'.

% @private Return the selected dispatch rule
get_dispatch_rule(#{user_data := #{dispatch_rule := DispatchRule}}) -> DispatchRule;
get_dispatch_rule(#{}) -> '$unknown'. 


% @private Return the duration in microseconds.
duration(undefined, _) -> undefined;
duration(_, undefined) -> undefined;
duration(Start, End) ->
    erlang:convert_time_unit(End-Start, native, microsecond).


% @private vm_stats
create_vm_metrics() ->
    ok = exometer:new([erlang, memory],
                      {function, erlang, memory, ['$dp'], value,
                       [total, processes, system, atom, binary, ets]}),

    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, port_count]}),

    ok = exometer:new([erlang, statistics],
                      {function, erlang, statistics, ['$dp'], value,
                       [run_queue]}),

    ok = exometer:new([erlang, gc],
                      {function, erlang, statistics, [garbage_collection],
                       match, {total_coll, rec_wrd, '_'}}),

    ok = exometer:new([erlang, io],
                      {function, erlang, statistics, [io], match,
                       {{'_', input}, {'_', output}}}).


% @private Setup mqtt reporter
setup_mqtt_reporter() ->
    add_mqtt_reporter(),

    [ ok = exometer_report:subscribe(z_exometer_mqtt,
                                     {select,
                                      [{ {[zotonic | '_'], DataPoint, enabled}, [], ['$_'] }]},
                                     default, % datapoints(DataPoint),
                                     10000)
      || DataPoint <- datapoints()],
    
    ok = exometer_report:subscribe(z_exometer_mqtt,
                                     {select,
                                      [{ {[erlang | '_'], '_', enabled}, [], ['$_'] }]},
                                     default,
                                     10000),

    ok.


%
add_mqtt_reporter() ->
    case exometer_report:add_reporter(
           z_exometer_mqtt, [{status, enabled}, {report_bulk, true}]) of
        ok -> ok;
        {error, already_running} -> ok
    end.


datapoints() ->
    [counter, spiral, gauge, histogram, meter].

datapoints(counter) ->[value];
datapoints(spiral) -> [count, one];
datapoints(gauge) -> [value];
datapoints(histogram) -> [mean, min, max, 50, 95, 99, 999];
datapoints(meter) -> [count, one, five, fifteen, day, mean].

