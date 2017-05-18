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
    ok.

init_site(Host) ->
    %% Webzmachine metrics
    exometer:re_register([zotonic, Host, webzmachine, requests], counter, []),
    exometer:re_register([zotonic, Host, webzmachine, duration], histogram, []),
    exometer:re_register([zotonic, Host, webzmachine, data_out], counter, []),

    exometer:re_register([zotonic, Host, depcache, evictions], counter, []),

    %% Database metrics
    exometer:re_register([zotonic, Host, db, requests], counter, []),
    exometer:re_register([zotonic, Host, db, duration], histogram, []),

    %% Session metrics
    exometer:re_register([zotonic, Host, session, sessions], gauge, []),
    exometer:re_register([zotonic, Host, session, page_processes], counter, []),

    ok.


%% @doc Collect log data from webzmachine and update webzmachine metrics
%%
log_access(_LogData) ->
    ok.

% log_access(#wm_log_data{finish_time=undefined}=LogData) ->
%     log_access(LogData#wm_log_data{finish_time=os:timestamp()});
% log_access(#wm_log_data{start_time=StartTime, finish_time=FinishTime,
%                         response_length=ResponseLength}=LogData) when StartTime =/= undefined ->
%     try
%         %% The request has already been counted by z_sites_dispatcher.
%         Host = webmachine_logger:get_metadata(zotonic_host, LogData),
%         exometer:update([zotonic, Host, webzmachine, duration], timer:now_diff(FinishTime, StartTime)),
%         exometer:update([zotonic, Host, webzmachine, data_out], ResponseLength)
%     after
%         z_access_syslog:log_access(LogData)
%     end.

