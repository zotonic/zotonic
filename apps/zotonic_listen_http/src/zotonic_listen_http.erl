%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Start the HTTP listener.

%% Copyright 2017 Marc Worrell
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

-module(zotonic_listen_http).

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
    start_http_listeners/0,
    stop_http_listeners/0,
    await/0
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(HTTP_REQUEST_TIMEOUT, 60000).

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
    ok = start_http_listeners(),
    {noreply, started}.

code_change(_Version, State, _Extra) ->
    {ok, State}.

terminate(_Why, started) ->
    stop_http_listeners();
terminate(_Why, init) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Stop all HTTP listeners
-spec stop_http_listeners() -> ok.
stop_http_listeners() ->
    lists:foreach(
            fun cowboy:stop_listener/1,
            [ zotonic_http_listener_ipv4,
              zotonic_https_listener_ipv4,
              zotonic_http_listener_ipv6,
              zotonic_https_listener_ipv6]).
%% @doc Start the HTTP/S listeners
-spec start_http_listeners() -> ok.
start_http_listeners() ->
    application:set_env(cowmachine, server_header, z_config:get(server_header)),
    ok = start_http_listeners_ip4(z_config:get(listen_ip), z_config:get(listen_port)),
    ok = start_https_listeners_ip4(z_config:get(listen_ip), z_config:get(ssl_listen_port)),
    case ipv6_supported() of
        true ->
            ok = start_http_listeners_ip6(z_config:get(listen_ip6), z_config:get(listen_port)),
            ok = start_https_listeners_ip6(z_config:get(listen_ip6), z_config:get(ssl_listen_port));
        false ->
            ok
    end.

%% @doc Start IPv4 HTTP listeners
start_http_listeners_ip4(none, _Port) ->
    lager:warning("HTTP IPv4 server disabled: 'listen_ip' is set to 'none'"),
    ok;
start_http_listeners_ip4(_WebIp, none) ->
    lager:warning("HTTP IPv4 server disabled: listen_port is set to 'none'"),
    ok;
start_http_listeners_ip4(WebIp, WebPort) ->
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    case cowboy:start_clear(
        zotonic_http_listener_ipv4,
        #{
            max_connections => z_config:get(max_connections),
            num_acceptors => z_config:get(inet_acceptor_pool_size),
            socket_opts => [
                inet,
                {port, WebPort},
                {backlog, z_config:get(inet_backlog)}
                | WebOpt
            ]
        },
        cowboy_options())
    of
        {ok, _} ->
            lager:info("HTTP IPv4 server listening on ~s:~p", [ip_to_string(WebIp), WebPort]),
            ok;
        {error, {already_started, _Pid}} ->
            lager:info("HTTP IPv4 server listening on ~s:~p", [ip_to_string(WebIp), WebPort]),
            ok;
        {error, eaddrinuse} ->
            lager:error("HTTP IPv4 server not started. Address in use on ~s:~p",
                        [ ip_to_string(WebIp), WebPort ]),
            {error, eaddrinuse};
        {error, Reason} ->
            lager:error("HTTP IPv4 server not started. Error ~p on ~s:~p",
                        [ Reason, ip_to_string(WebIp), WebPort ]),
            {error, Reason}
    end.

%% @doc Start IPv4 HTTPS listeners
start_https_listeners_ip4(none, _SSLPort) ->
    lager:info("HTTPS IPv4 server disabled: 'ssl_listen_ip' is set to 'none'"),
    ok;
start_https_listeners_ip4(_WebIp, none) ->
    lager:info("HTTPS IPv4 server disabled: 'ssl_listen_port' is set to 'none'"),
    ok;
start_https_listeners_ip4(WebIp, SSLPort) ->
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    case cowboy:start_tls(
        zotonic_https_listener_ipv4,
        #{
            max_connections => z_config:get(ssl_max_connections),
            num_acceptors => z_config:get(ssl_acceptor_pool_size),
            socket_opts => [
                inet,
                {port, SSLPort},
                {backlog, z_config:get(ssl_backlog)}
            ]
            ++ z_ssl_certs:ssl_listener_options()
            ++ WebOpt
        },
        cowboy_options())
    of
        {ok, _} ->
            lager:info("HTTPS IPv4 server listening on ~s:~p", [ip_to_string(WebIp), SSLPort]),
            ok;
        {error, {already_started, _Pid}} ->
            lager:info("HTTPS IPv4 server listening on ~s:~p", [ip_to_string(WebIp), SSLPort]),
            ok;
        {error, eaddrinuse} ->
            lager:error("HTTPS IPv4 server not started. Address in use on ~s:~p",
                        [ ip_to_string(WebIp), SSLPort ]),
            {error, eaddrinuse};
        {error, Reason} ->
            lager:error("HTTPS IPv4 server not started. Error ~p on ~s:~p",
                        [ Reason, ip_to_string(WebIp), SSLPort ]),
            {error, Reason}
    end.

%% @doc As we don't have a separate listen_ipv6 config yet, only start ip6 on 'any'
start_http_listeners_ip6(none, _WebPort) ->
    ok;
start_http_listeners_ip6(_WebIp, none) ->
    ok;
start_http_listeners_ip6(WebIp, WebPort) ->
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    case cowboy:start_clear(
        zotonic_http_listener_ipv6,
        #{
            max_connections => z_config:get(max_connections),
            num_acceptors => z_config:get(inet_acceptor_pool_size),
            socket_opts => [
                inet6,
                {ipv6_v6only, true},
                {port, WebPort},
                {backlog, z_config:get(inet_backlog)}
            ]
            ++ WebOpt
        },
        cowboy_options())
    of
        {ok, _} ->
            lager:info("HTTP IPv6 server listening on ~s:~p", [ ip_to_string(WebIp), WebPort ]),
            ok;
        {error, {already_started, _Pid}} ->
            lager:info("HTTP IPv6 server listening on ~s:~p", [ ip_to_string(WebIp), WebPort ]),
            ok;
        {error, eaddrinuse} ->
            lager:error("HTTP IPv6 server not started. Address in use on ~s:~p",
                        [ ip_to_string(WebIp), WebPort ]),
            {error, eaddrinuse};
        {error, Reason} ->
            lager:error("HTTP IPv6 server not started. Error on ~p ~s:~p",
                        [ Reason, ip_to_string(WebIp), WebPort ]),
            {error, Reason}
    end.

%% @doc Start the ip6 HTTPS listener
start_https_listeners_ip6(none, _SSLPort) ->
    ok;
start_https_listeners_ip6(_WebIp, none) ->
    ok;
start_https_listeners_ip6(WebIp, SSLPort) ->
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    case cowboy:start_tls(
        zotonic_https_listener_ipv6,
        #{
            max_connections => z_config:get(ssl_max_connections),
            num_acceptors => z_config:get(ssl_acceptor_pool_size),
            socket_opts => [
                inet6,
                {ipv6_v6only, true},
                {port, SSLPort},
                {backlog, z_config:get(ssl_backlog)}
            ]
            ++ z_ssl_certs:ssl_listener_options()
            ++ WebOpt
        },
        cowboy_options())
    of
        {ok, _} ->
            lager:info("HTTPS IPv6 server listening on ~s:~p", [ip_to_string(WebIp), SSLPort]),
            ok;
        {error, {already_started, _Pid}} ->
            lager:info("HTTPS IPv6 server listening on ~s:~p", [ip_to_string(WebIp), SSLPort]),
            ok;
        {error, eaddrinuse} ->
            lager:error("HTTPS IPv6 server not started. Address in use on ~p:~p",
                        [ WebIp, SSLPort ]),
            {error, eaddrinuse};
        {error, Reason} ->
            lager:error("HTTPS IPv6 server not started. Error on ~p ~s:~p",
                        [ Reason, ip_to_string(WebIp), SSLPort ]),
            {error, Reason}
    end.

ip_to_string(any) -> "any";
ip_to_string(IP) -> inet:ntoa(IP).

cowboy_options() ->
    #{
        middlewares => [ cowmachine_proxy, z_sites_dispatcher, z_cowmachine_middleware ],
        stream_handlers => [ cowboy_metrics_h, cowboy_stream_h ],
        request_timeout => ?HTTP_REQUEST_TIMEOUT,
        metrics_callback => fun zotonic_listen_http_metrics:metrics_callback/1,
        env => #{}
    }.

%% @todo Exclude platforms that do not support raw ipv6 socket options
-spec ipv6_supported() -> boolean().
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.
