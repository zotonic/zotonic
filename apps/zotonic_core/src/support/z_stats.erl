%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2013-2021 Maas-Maarten Zeeman
%% @doc Module for handling request statistics.

%% Copyright 2013-2021 Maas-Maarten Zeeman
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
-export([
    log_access/1,
    count_db_event/2,

    record_event/3,
    record_duration/4,
    system_usage/1
]).


%% @doc Initialize the statistics collection machinery.
%%
init() ->
    create_vm_metrics(),
    setup_system_reporter(),

    ok.

% @doc Setup stats for each site.
init_site(Host) ->
    % %% Cowmachine/HTTP metrics
    % % exometer:re_register([zotonic, Host, http, requests], counter, []),
    % % exometer:re_register([zotonic, Host, http, duration], histogram, []),
    % % exometer:re_register([zotonic, Host, http, data_out], counter, []),

    % %% MQTT metrics
    % exometer:re_register([zotonic, Host, mqtt, connects], counter, []),

    % %% Misc metrics
    % exometer:re_register([zotonic, Host, depcache, evictions], spiral, []),

    % %% Database metrics
    % exometer:re_register([zotonic, Host, db, requests], spiral, []),
    % exometer:re_register([zotonic, Host, db, pool_full], spiral, []),
    % exometer:re_register([zotonic, Host, db, pool_high_usage], spiral, []),
    % exometer:re_register([zotonic, Host, db, duration], histogram, []),
    % exometer:re_register([zotonic, Host, db, connection_wait], histogram, []),
    Context = z_context:new(Host),

    % Keep track of the size of the depcache
    ok = exometer:re_register([site, Host, depcache, size],
                              {function, z_depcache, size, [Context], match, size}, []),

    % And some basic broker statistics
    ok = exometer:re_register([site, Host, broker, session_count],
                              {function, mqtt_sessions, session_count, [Host], match, count}, []),

    ok = exometer:re_register([site, Host, broker, router_info],
                              {function, mqtt_sessions, router_info, [Host], value, []}, []),

    ok.

% @doc Count a event
record_event(System, What, #context{}=Context) ->
    record_event(System, What, z_context:site(Context));
record_event(System, What, Site) when is_atom(Site) ->
    ok = exometer:update_or_create([site, Site, System, What], 1, spiral, []).

% @doc Record a duration
record_duration(System, What, Duration, #context{}=Context) ->
    Site = z_context:site(Context),
    record_duration(System, What, Duration, Site);
record_duration(System, What, Duration, Site) when is_atom(Site) ->
    record_event(System, What, Site),
    ok = exometer:update_or_create([site, Site, System, What, duration], Duration, histogram, []).


%% @doc Collect log data from cowmachine and update cowmachine metrics
%%

log_access(MetricsData) ->
    try
        handle_cowmachine_stats(MetricsData)
    after
        z_access_syslog:log_access(MetricsData)
    end.


% @private Register the request.
handle_cowmachine_stats(MetricsData) ->
    Site = get_site(MetricsData),
    DispatchRule = get_dispatch_rule(MetricsData),

    Duration = duration(maps:get(req_start, MetricsData, undefined),
                        maps:get(resp_end, MetricsData, undefined)),

    DataIn = maps:get(req_body_length, MetricsData),
    DataOut = maps:get(resp_body_length, MetricsData),

    Reason = maps:get(reason, MetricsData),
    Status = maps:get(resp_status, MetricsData),

    StatusCategory = http_status_category(Reason, Status),

    PathPrefix = [site, Site, cowmachine, DispatchRule],

    ok = exometer:update_or_create(PathPrefix ++ [StatusCategory], 1, spiral, []),

    if Duration > 0 ->
           ok = exometer:update_or_create(PathPrefix ++ [duration], Duration, histogram, []);
       Duration =< 0 ->
           ok
    end,

    if DataIn > 0 ->
           ok = exometer:update_or_create(PathPrefix ++ [data_in], DataIn, spiral, []);
       DataIn =< 0 ->
           ok
    end,

    if DataOut > 0 ->
           ok = exometer:update_or_create(PathPrefix ++ [data_out], DataOut, spiral, []);
       DataOut =< 0 ->
           ok
    end,

    ok.

% log_access(#wm_log_data{finish_time=undefined}=LogData) ->
%     log_access(LogData#wm_log_data{finish_time=os:timestamp()});
% log_access(#wm_log_data{start_time=StartTime, finish_time=FinishTime,
%                         response_length=ResponseLength}=LogData) when StartTime =/= undefined ->
%     try
%         %% The request has already been counted by z_sites_dispatcher.
%         Host = webmachine_logger:get_metadata(zotonic_host, LogData),
%         exometer:update([zotonic, Host, http, duration], timer:now_diff(FinishTime, StartTime)),
%         exometer:update([zotonic, Host, http, data_out], ResponseLength)
%     after
%         z_access_syslog:log_access(LogData)
%     end.

%% @doc Count a db event, like pool_full, or pull_high_usage.
count_db_event(Event, Context) when is_atom(Event) ->
    Site = z_context:site(Context),
    ok = exometer:update_or_create([site, Site, db, Event]), 1, spiral, []).

% Return the usage in percentage, for atoms, ports and processes.
system_usage(atom) -> system_usage_helper(atom_count, atom_limit);
system_usage(port) -> system_usage_helper(port_count, port_limit);
% system_usage(ets) -> system_usage_helper(ets_count, ets_limit);  % from 21 and up
system_usage(process) -> system_usage_helper(process_count, process_limit);
% Memory
system_usage(ets_memory) -> system_memory_usage_helper(ets);
system_usage(code_memory) -> system_memory_usage_helper(code);
system_usage(process_memory) -> system_memory_usage_helper(processes);
system_usage(binary_memory) -> system_memory_usage_helper(binary);
system_usage(atom_memory) -> system_memory_usage_helper(atom).

%% Returns the usage in percentage
system_usage_helper(Count, Limit) ->
    z_convert:to_integer((erlang:system_info(Count) / erlang:system_info(Limit)) * 100).

%% Returns the memory usage in percentage
system_memory_usage_helper(Type) ->
    z_convert:to_integer((erlang:memory(Type) / erlang:memory(total)) * 100).


%%
%% Helpers
%%

% @private return a status category
http_status_category(switch_protocol, _) -> '1xx';
http_status_category(_, X) when X < 300 -> '2xx';
http_status_category(_, X) when X < 400 -> '3xx';
http_status_category(_, X) when X < 500 -> '4xx';
http_status_category(_, X) when X < 600 -> '5xx';
http_status_category(_, _X) -> 'xxx'.


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
                      {function, erlang, memory, [], value, []}),

    ok = exometer:new([erlang, usage],
                      {function, z_stats, system_usage, ['$dp'], value,
                       [atom, process, port, 
                        ets_memory, binary_memory, code_memory, process_memory, atom_memory]}),

    ok = exometer:new([erlang, system],
                      {function, erlang, system_info, ['$dp'], value,
                       [process_count, process_limit,
                        port_count, port_limit,
                        atom_count, atom_limit
                       ]}),

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
setup_system_reporter() ->
    add_system_reporter(),

    ok = exometer_report:subscribe(system_reporter,
                                   {select,
                                    [{ {[site | '_'], '_', enabled}, [], ['$_'] }]},
                                   default,
                                   10000),
    
    ok = exometer_report:subscribe(system_reporter,
                                   {select,
                                    [{ {[erlang | '_'], '_', enabled}, [], ['$_'] }]},
                                   default,
                                   10000),

    ok.


% Add the system reporter. It publishes on the default mqtt pool for
% the whole system.
add_system_reporter() -> 
    case exometer_report:add_reporter(
           system_reporter,
           [ {module, z_exometer_mqtt},
             {status, enabled},
             {report_bulk, true},
             {topic_prefix, [<<"$SYS">>]},
             {context, #context{ site = '-mqtt-',
                                 acl = admin }} ]) of
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
