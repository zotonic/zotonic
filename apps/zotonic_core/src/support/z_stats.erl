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
    count_db_event/2
]).

%% @doc Initialize the statistics collection machinery.
%%
init() ->
    ok.

init_site(Host) ->
    %% Cowmachine/HTTP metrics
    exometer:re_register([zotonic, Host, http, requests], counter, []),
    exometer:re_register([zotonic, Host, http, duration], histogram, []),
    exometer:re_register([zotonic, Host, http, data_out], counter, []),

    %% MQTT metrics
    exometer:re_register([zotonic, Host, mqtt, connects], counter, []),

    %% Misc metrics
    exometer:re_register([zotonic, Host, depcache, evictions], counter, []),

    %% Database metrics
    exometer:re_register([zotonic, Host, db, requests], spiral, []),
    exometer:re_register([zotonic, Host, db, pool_full], spiral, []),
    exometer:re_register([zotonic, Host, db, pool_high_usage], spiral, []),
    exometer:re_register([zotonic, Host, db, duration], histogram, []),
    exometer:re_register([zotonic, Host, db, connection_wait], histogram, []),

    %% Session metrics
    exometer:re_register([zotonic, Host, session, sessions], gauge, []),
    exometer:re_register([zotonic, Host, session, page_processes], counter, []),

    ok.


%% @doc Collect log data from webzmachine and update webzmachine metrics
%%

log_access(MetricsData) ->
    Site = get_site(MetricsData),

    try
        count_request(Site),
        measure_duration(Site,
                         duration(maps:get(req_start, MetricsData, undefined),
                                  maps:get(resp_end, MetricsData, undefined))),
        measure_data_out(Site, maps:get(resp_body_length, MetricsData))
    after
        z_access_syslog:log_access(MetricsData)
    end.


count_request(Site) when is_atom(Site) ->
    exometer:update([zotonic, Site, webzmachine, requests], 1).

measure_duration(Site, Duration) when is_integer(Duration) ->
    exometer:update([zotonic, Site, webzmachine, duration], Duration);
measure_duration(_Site, _) ->
    ok.

measure_data_out(Site, DataOut) when is_integer(DataOut) ->
    exometer:update([zotonic, Site, webzmachine, data_out], DataOut);
measure_data_out(_Site, _DataOut) ->
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
    ok = exometer:update_or_create([zotonic, Site, db, Event], 1, spiral, []).

%%
%% Helpers
%%

% @private Return the site name from the user-data
get_site(#{user_data := #{site := Site}}) -> Site;
get_site(#{}) -> undefined.

% @private Return the duration in microseconds.
duration(undefined, _) -> undefined;
duration(_, undefined) -> undefined;
duration(Start, End) ->
    erlang:convert_time_unit(End-Start, native, microsecond).
