%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Start the MQTT listener.

%% Copyright 2019 Marc Worrell
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

-module(zotonic_listen_mqtt).

-behaviour(gen_server).

-export([start_link/0]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
    ]).

-export([
    start_mqtt_listeners/0,
    stop_mqtt_listeners/0,
    await/0
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MQTT_REQUEST_TIMEOUT, 60000).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec await() -> ok.
await() ->
    case gen_server:call(?MODULE, status, infinity) of
        started -> ok;
        init ->
            timer:sleep(10),
            await()
    end.

%%====================================================================
%% gen_server functions
%%====================================================================


init([]) ->
    % Set trap_exit, so that terminate is called
    process_flag(trap_exit, true),
    self() ! start,
    {ok, init}.

handle_call(status, _From, State) ->
    {reply, State, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(start, init) ->
    ok = start_mqtt_listeners(),
    {noreply, started}.

code_change(_Version, State, _Extra) ->
    {noreply, State}.

terminate(_Why, started) ->
    stop_mqtt_listeners();
terminate(_Why, init) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Stop all MQTT listeners
-spec stop_mqtt_listeners() -> ok.
stop_mqtt_listeners() ->
    lists:foreach(
            fun cowboy:stop_listener/1,
            [ zotonic_mqtt_listener_ipv4,
              zotonic_mqtts_listener_ipv4,
              zotonic_mqtt_listener_ipv6,
              zotonic_mqtts_listener_ipv6]).

%% @doc Start the MQTT/S listeners
-spec start_mqtt_listeners() -> ok.
start_mqtt_listeners() ->
    z_ssl_certs:ensure_dhfile(),
    start_mqtt_listeners_ip4(z_config:get(mqtt_listen_ip), z_config:get(mqtt_listen_port)),
    start_mqtts_listeners_ip4(z_config:get(mqtt_listen_ip), z_config:get(mqtt_listen_ssl_port)),
    case ipv6_supported() of
        true ->
            start_mqtt_listeners_ip6(z_config:get(mqtt_listen_ip6), z_config:get(mqtt_listen_port)),
            start_mqtts_listeners_ip6(z_config:get(mqtt_listen_ip6), z_config:get(mqtt_listen_ssl_port)),
            ok;
        false ->
            ok
    end.

%% @doc Start IPv4 MQTT listeners
start_mqtt_listeners_ip4(none, _Port) ->
    lager:warning("MQTT server disabled: 'mqtt_listen_ip' is set to 'none'"),
    ignore;
start_mqtt_listeners_ip4(_WebIp, none) ->
    lager:warning("MQTT server disabled: 'mqtt_listen_port' is set to 'none'"),
    ignore;
start_mqtt_listeners_ip4(WebIp, WebPort) ->
    lager:info("MQTT server listening on IPv4 ~s:~p", [ip_to_string(WebIp), WebPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    case ranch:start_listener(
        zotonic_mqtt_listener_ipv4,
        ranch_tcp,
        [   inet,
            {port, WebPort},
            {backlog, z_config:get(inet_backlog)},
            {num_acceptors, z_config:get(inet_acceptor_pool_size)},
            {max_connections, z_config:get(mqtt_max_connections)}
            | WebOpt
        ],
        zotonic_listen_mqtt_handler,
        handler_options())
    of
        {ok, _} = OK -> OK;
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% @doc Start IPv4 MQTT ssl listeners
start_mqtts_listeners_ip4(none, _SSLPort) -> ignore;
start_mqtts_listeners_ip4(_WebIp, none) ->
    lager:info("MQTT ssl server disabled: 'mqtt_listen_ssl_port' is set to 'none'"),
    ignore;
start_mqtts_listeners_ip4(WebIp, SSLPort) ->
    lager:info("MQTT ssl server listening on IPv4 ~s:~p", [ip_to_string(WebIp), SSLPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    case ranch:start_listener(
        zotonic_mqtts_listener_ipv4,
        ranch_ssl,
        [   inet,
            {port, SSLPort},
            {backlog, z_config:get(ssl_backlog)},
            {num_acceptors, z_config:get(ssl_acceptor_pool_size)},
            {max_connections, z_config:get(mqtt_ssl_max_connections)}
        ]
        ++ z_ssl_certs:ssl_listener_options()
        ++ WebOpt,
        zotonic_listen_mqtt_handler_tls,
        handler_options())
    of
        {ok, _} = OK -> OK;
        {error, {already_started, Pid}} -> {ok, Pid}
    end.

%% @doc As we don't have a separate listen_ipv6 config yet, only start ip6 on 'any'
start_mqtt_listeners_ip6(none, _WebPort) -> ignore;
start_mqtt_listeners_ip6(_WebIp, none) -> ignore;
start_mqtt_listeners_ip6(WebIp, WebPort) ->
    lager:info("MQTT server listening on IPv6 ~s:~p", [ip_to_string(WebIp), WebPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    {ok, _} = ranch:start_listener(
        zotonic_mqtt_listener_ipv6,
        ranch_tcp,
        [   inet6,
            {ipv6_v6only, true},
            {port, WebPort},
            {backlog, z_config:get(inet_backlog)},
            {num_acceptors, z_config:get(inet_acceptor_pool_size)},
            {max_connections, z_config:get(mqtt_max_connections)}
        ]
        ++ WebOpt,
        zotonic_listen_mqtt_handler,
        handler_options()).

%% @doc Start the ip6 MQTT ssl listener
start_mqtts_listeners_ip6(none, _SSLPort) -> ignore;
start_mqtts_listeners_ip6(_WebIp, none) -> ignore;
start_mqtts_listeners_ip6(WebIp, SSLPort) ->
    lager:info("MQTT ssl server listening on IPv6 ~s:~p", [ip_to_string(WebIp), SSLPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    {ok, _} = ranch:start_listener(
        zotonic_mqtts_listener_ipv6,
        ranch_ssl,
        [   inet6,
            {ipv6_v6only, true},
            {port, SSLPort},
            {backlog, z_config:get(ssl_backlog)},
            {num_acceptors, z_config:get(ssl_acceptor_pool_size)},
            {max_connections, z_config:get(mqtt_ssl_max_connections)}
        ]
        ++ z_ssl_certs:ssl_listener_options()
        ++ WebOpt,
        zotonic_listen_mqtt_handler_tls,
        handler_options()).

ip_to_string(any) -> "any";
ip_to_string(IP) -> inet:ntoa(IP).

handler_options() ->
    #{ }.


%% @todo Exclude platforms that do not support raw ipv6 socket options
-spec ipv6_supported() -> boolean().
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.
