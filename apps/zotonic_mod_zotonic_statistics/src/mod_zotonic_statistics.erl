%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%% @doc Registers zotonic and system metrics

%% Copyright 2015 Maas-Maarten Zeeman
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

-module(mod_zotonic_statistics).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_title("Zotonic Statistics").
-mod_description("Registers system wide zotonic metrics.").
-mod_prio(800).


%% interface functions
-export([
    observe_module_activate/2,
    observe_module_deactivate/2,
    get_tcp_port_count/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-type metric() :: { exometer:name(), exometer:type(), exometer:options() }.

-type subscription() :: {
    exometer_report:metric(),
    [ exometer_report:datapoint() ],
    exometer_report:interval(),
    exometer_report:extra(),
    boolean() }.

%%
%% Observes
%%

observe_module_activate(#module_activate{ module = ?MODULE }, _Context) ->
    % ensure_metrics(erlang_metrics()),
    % ensure_z_exometer_mqtt_added(),
    % ensure_subscriptions(erlang_subscriptions()),
    ok;
observe_module_activate(#module_activate{}, _Context) ->
    ok.

observe_module_deactivate(#module_deactivate{ module = ?MODULE }, _Context) ->
    lists:foreach(
        fun( {Metric, DataPoint, _, _, _} ) ->
            exometer_report:unsubscribe(z_exometer_mqtt, Metric, DataPoint)
        end,
        erlang_subscriptions());
observe_module_deactivate(#module_deactivate{}, _Context) ->
    ok.

%%
%% Helpers
%%

-spec erlang_metrics() -> list( metric() ).
erlang_metrics() ->
    [
        {[erlang, memory], {function, erlang, memory, [],
                proplist, [total, processes, system, atom, binary, code, ets]}, [{cache, 500}]},
        {[erlang, system_info], {function, erlang, system_info, ['$dp'],
                value, [process_limit, process_count, port_limit, port_count]}, []},
        {[erlang, statistics], {function, erlang, statistics, ['$dp'],
                value, [run_queue]}, []},
        {[erlang, io], {function, erlang, statistics, [io],
                match, {{'_', input}, {'_', output}}}, []},
        {[erlang, network], {function, mod_zotonic_statistics, get_tcp_port_count, [],
                value, [tcp_port_count]}, []}
    ].

-spec erlang_subscriptions() -> list( subscription() ).
erlang_subscriptions() ->
    [
        {[erlang, memory],
            [total, ets, binary, processes, system, atom, code], 10000, undefined, true},
        {[erlang, system_info],
            [process_count, process_limit, port_count, port_limit], 10000, undefined, true},
        {[erlang, statistics],
            [run_queue], 10000, undefined, true},
        {[erlang, io],
            [input, output], 10000, undefined, true},
        {[erlang, network],
            [tcp_port_count], 10000, undefined, true}
    ].

ensure_metrics([]) -> ok;
ensure_metrics([{Name, Type, Opts}|Rest]) ->
    ensure_metric(Name, Type, Opts),
    ensure_metrics(Rest).

-spec ensure_metric(  exometer:name(), exometer:type(), exometer:options() ) -> ok.
ensure_metric(Name, Type, Opts) ->
    case catch exometer:ensure(Name, Type, Opts) of
        ok ->
            ok;
        {'EXIT', {type_mismatch, _}} ->
            ok = exometer:re_register(Name, Type, Opts)
    end.

ensure_z_exometer_mqtt_added() ->
    case exometer_report:add_reporter(z_exometer_mqtt, []) of
        ok -> ok;
        {error, already_running} -> ok
    end.

ensure_subscriptions([]) -> ok;
ensure_subscriptions([{Metric, DataPoint, Interval, Extra, Retry}|Rest]) ->
    ok = exometer_report:subscribe(z_exometer_mqtt, Metric, DataPoint, Interval, Extra, Retry),
    ensure_subscriptions(Rest).

get_tcp_port_count() ->
    [{tcp_port_count, length(recon:tcp())}].
