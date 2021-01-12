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

-behaviour(gen_server).

-export([init_system/0, init_site/1]).

-export([
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

%% Act as a webmachine logger
-export([
    log_access/1,
    record_event/3,
    record_count/4,
    record_duration/4,
    system_usage/1
]).

-record(state, {
    backoff = 0 :: non_neg_integer(),
    buffers :: list( atom() )
}).

%% Initial delay after start before consuming the buffers.
-define(INITIAL_DELAY, 100).

%% Backoff if no logging happening, values in msecs.
-define(BACKOFF_QUIET, 10).
-define(BACKOFF_INCREMENT, 10).
-define(BACKOFF_MAX, 2000).

%% Batch size of metrics handling per poll
-define(BATCH_SIZE, 200).


%% -------------------------------------------------------------------------------------------------
%% API functions
%% -------------------------------------------------------------------------------------------------

%% @doc Start the log buffer consumer.
start_link(Buffers) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Buffers, []).

%% @doc Initialize the statistics collection machinery.
init_system() ->
    create_vm_metrics(),
    setup_system_reporter(),
    ok.

% @doc Setup stats for each site.
init_site(Host) ->
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

% @doc Count some amount, like data transfers
record_count(System, What, Count, #context{}=Context) ->
    record_count(System, What, Count, z_context:site(Context));
record_count(System, What, Count, Site) when is_atom(Site) ->
    ok = exometer:update_or_create([site, Site, System, What], Count, spiral, []).

% @doc Record a duration
record_duration(System, What, Duration, #context{}=Context) ->
    Site = z_context:site(Context),
    record_duration(System, What, Duration, Site);
record_duration(System, What, Duration, Site) when is_atom(Site) ->
    record_event(System, What, Site),
    ok = exometer:update_or_create([site, Site, System, What, duration], Duration, histogram, []).

%% -------------------------------------------------------------------------------------------------
%% gen_server callbacks
%% -------------------------------------------------------------------------------------------------

init(Buffers) ->
    {ok, #state{ buffers = Buffers }, ?INITIAL_DELAY}.

handle_call(_Cmd, _From, State) ->
    {stop, not_implemented, State}.

handle_cast(_Cmd, State) ->
    {stop, not_implemented, State}.

handle_info(timeout, #state{ buffers = Buffers } = State) ->
    % Poll the buffers, start with draining the first one, then drain
    % the other buffers.
    % If drained, then increment backoff counter (till max)
    % If entries found, reset backoff counter.
    maybe_backoff(drain_buffers(Buffers, 0), State).

maybe_backoff({empty, 0}, #state{ backoff = BackOff } = State) ->
    BackOff1 = erlang:max(BackOff + ?BACKOFF_INCREMENT, ?BACKOFF_MAX),
    {noreply, State#state{ backoff = BackOff1 }, BackOff1};
maybe_backoff({empty, _Count}, State) ->
    {noreply, State#state{ backoff = 0 }, ?BACKOFF_QUIET};
maybe_backoff({full, _Count}, State) ->
    {noreply, State#state{ backoff = 0 }, 0}.

%% -------------------------------------------------------------------------------------------------
%% local functions
%% -------------------------------------------------------------------------------------------------

drain_buffers([], Count) ->
    {empty, Count};
drain_buffers([ Buffer | Buffers  ] = Bs, Count) when Count < ?BATCH_SIZE ->
    case ringbuffer:read(Buffer) of
        {ok, {Skipped, Metric}} ->
            report_skipped(Skipped),
            log_access(Metric),
            drain_buffers(Bs, Count+1);
        {error, empty} ->
            drain_buffers(Buffers, Count)
    end;
drain_buffers(_, Count) ->
    {full, Count}.


report_skipped(0) -> ok;
report_skipped(Count) -> record_count(stats, skipped, Count, zotonic).


%% @doc Collect log data from cowmachine and update cowmachine metrics
%%
%% The log entry from zotonic_listen_http_metrics is:
%% <code>
%% Log = #{
%%     site => Site,
%%     reason => Reason,
%%     req_start => ReqStart,
%%     duration_total_usec => DurationTotalUsec,
%%     duration_process_usec => DurationProcessUsec,
%%     resp_status => RespStatus,
%%     resp_status_category => StatusCategory,
%%     req_bytes => ReqBodyLength,
%%     resp_bytes => RespBodyLength,
%%     http_version => cowboy_req:version(Req),
%%     method => cowboy_req:method(Req),
%%     path => cowboy_req:path(Req),
%%     user_agent => cowboy_req:header(<<"user-agent">>, Req),
%%     referer => cowboy_req:header(<<"referer">>, Req),
%%     metrics => UserData#{ peer_ip => PeerIP }
%% }
%% </code>
log_access(#{
        site := Site,
        req_start := ReqStart,
        resp_status := Status,
        resp_status_category := StatusCat,
        method := Method
    } = MetricsData) when Site =/= undefined, is_atom(Site) ->
    try
        handle_cowmachine_stats(MetricsData)
    after
        Context = z_context:new(Site, 'en', <<"UTC">>),
        Msg = #http_log_access{
            timestamp = monotonic_time_to_timestamp(ReqStart),
            status = Status,
            status_category = StatusCat,
            method = Method,
            metrics = MetricsData
        },
        z_notifier:notify_sync(Msg, Context)
    end;
log_access(_Metrics) ->
    ok.

monotonic_time_to_timestamp(MonotonicTime) ->
    Time = erlang:convert_time_unit(MonotonicTime, native, second) + erlang:time_offset(second),
    MegaSecs = Time div 1000000,
    Secs = Time rem 1000000,
    {MegaSecs, Secs, 0}.

% @private Register the request.
handle_cowmachine_stats(#{
        site := Site,
        duration_process_usec := DurationUSec,
        resp_status_category := StatusCategory,
        req_bytes := DataIn,
        resp_bytes := DataOut,
        metrics := Metrics
    }) ->
    DispatchRule = maps:get(dispatch_rule, Metrics, undefined),
    PathPrefix = [site, Site, cowmachine, DispatchRule],
    exometer:update_or_create(PathPrefix ++ [StatusCategory], 1, spiral, []),
    if DurationUSec > 0 ->
           exometer:update_or_create(PathPrefix ++ [duration], DurationUSec, histogram, []);
       true ->
           ok
    end,
    case DataIn of
        undefined -> ok;
        0 -> ok;
        _ -> exometer:update_or_create(PathPrefix ++ [data_in], DataIn, spiral, [])
    end,
    case DataOut of
        undefined -> ok;
        0 -> ok;
        _ -> exometer:update_or_create(PathPrefix ++ [data_out], DataOut, spiral, [])
    end,
    ok.

% Return the usage in percentage, for atoms, ports and processes.
system_usage(atom) -> system_usage_helper(atom_count, atom_limit);
system_usage(port) -> system_usage_helper(port_count, port_limit);
system_usage(ets) -> system_usage_helper(ets_count, ets_limit);
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
            [
                {module, z_exometer_mqtt},
                {status, enabled},
                {report_bulk, true},
                {topic_prefix, [<<"$SYS">>]},
                {context, #context{ site = '-mqtt-', acl = admin }}
            ])
    of
        ok -> ok;
        {error, already_running} -> ok
    end.

% datapoints() ->
%     [counter, spiral, gauge, histogram, meter].

% datapoints(counter) ->[value];
% datapoints(spiral) -> [count, one];
% datapoints(gauge) -> [value];
% datapoints(histogram) -> [mean, min, max, 50, 95, 99, 999];
% datapoints(meter) -> [count, one, five, fifteen, day, mean].
