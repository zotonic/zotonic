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
    {noreply, State}.

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
    z_ssl_certs:ensure_dhfile(),
    application:set_env(cowmachine, server_header,
        <<"Zotonic/", (z_convert:to_binary(?ZOTONIC_VERSION))/binary>>),
    start_http_listeners_ip4(z_config:get(listen_ip), z_config:get(listen_port)),
    start_https_listeners_ip4(z_config:get(listen_ip), z_config:get(ssl_listen_port)),
    case ipv6_supported() of
        true ->
            start_http_listeners_ip6(z_config:get(listen_ip6), z_config:get(listen_port)),
            start_https_listeners_ip6(z_config:get(listen_ip6), z_config:get(ssl_listen_port)),
            ok;
        false ->
            ok
    end.

%% @doc Start IPv4 HTTP listeners
start_http_listeners_ip4(none, _Port) ->
    lager:warning("HTTP server disabled: 'listen_ip' is set to 'none'"),
    ignore;
start_http_listeners_ip4(_WebIp, none) ->
    lager:warning("HTTP server disabled: listen_port is set to 'none'"),
    ignore;
start_http_listeners_ip4(WebIp, WebPort) ->
    lager:info("HTTP server listening on IPv4 ~s:~p", [ip_to_string(WebIp), WebPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    {ok, _} = cowboy:start_clear(
        zotonic_http_listener_ipv4,
        [   inet,
            {port, WebPort},
            {backlog, z_config:get(inet_backlog)},
            {num_acceptors, z_config:get(inet_acceptor_pool_size)}
            | WebOpt
        ],
        cowboy_options()).

%% @doc Start IPv4 HTTPS listeners
start_https_listeners_ip4(none, _SSLPort) -> ignore;
start_https_listeners_ip4(_WebIp, none) ->
    lager:info("HTTPS server disabled: 'ssl_listen_port' is set to 'none'"),
    ignore;
start_https_listeners_ip4(WebIp, SSLPort) ->
    lager:info("HTTPS server listening on IPv4 ~s:~p", [ip_to_string(WebIp), SSLPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    {ok, _} = start_tls(
        zotonic_https_listener_ipv4,
        [   inet,
            {port, SSLPort},
            {backlog, z_config:get(ssl_backlog)},
            {num_acceptors, z_config:get(ssl_acceptor_pool_size)}
        ]
        ++ z_ssl_certs:ssl_listener_options()
        ++ WebOpt,
        cowboy_options()).

%% @doc As we don't have a separate listen_ipv6 config yet, only start ip6 on 'any'
start_http_listeners_ip6(none, _WebPort) -> ignore;
start_http_listeners_ip6(_WebIp, none) -> ignore;
start_http_listeners_ip6(WebIp, WebPort) ->
    lager:info("HTTP server listening on IPv6 ~s:~p", [ip_to_string(WebIp), WebPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    {ok, _} = cowboy:start_clear(
        zotonic_http_listener_ipv6,
        [   inet6,
            {ipv6_v6only, true},
            {port, WebPort},
            {backlog, z_config:get(inet_backlog)},
            {num_acceptors, z_config:get(inet_acceptor_pool_size)}
        ]
        ++ WebOpt,
        cowboy_options()).

%% @doc Start the ip6 HTTPS listener
start_https_listeners_ip6(none, _SSLPort) -> ignore;
start_https_listeners_ip6(_WebIp, none) -> ignore;
start_https_listeners_ip6(WebIp, SSLPort) ->
    lager:info("HTTPS server listening on IPv6 ~s:~p", [ip_to_string(WebIp), SSLPort]),
    WebOpt = case WebIp of
        any -> [];
        _ -> [{ip, WebIp}]
    end,
    {ok, _} = start_tls(
        zotonic_https_listener_ipv6,
        [   inet6,
            {ipv6_v6only, true},
            {port, SSLPort},
            {backlog, z_config:get(ssl_backlog)},
            {num_acceptors, z_config:get(ssl_acceptor_pool_size)}
        ]
        ++ z_ssl_certs:ssl_listener_options()
        ++ WebOpt,
        cowboy_options()).

ip_to_string(any) -> "any";
ip_to_string(IP) -> inet:ntoa(IP).

cowboy_options() ->
    #{
        middlewares => [ cowmachine_proxy, z_sites_dispatcher, z_cowmachine_middleware ],
        request_timeout => ?HTTP_REQUEST_TIMEOUT,
        env => #{}
    }.


% @doc Copied from cowboy.erl, disable http2 till the cipher problems are resolved.
-spec start_tls(ranch:ref(), ranch_ssl:opts(), list()) -> {ok, pid()} | {error, any()}.
start_tls(Ref, TransOpts0, ProtoOpts) ->
    TransOpts = [
        connection_type(ProtoOpts)
        % {next_protocols_advertised, [<<"h2">>, <<"http/1.1">>]},
        % {alpn_preferred_protocols, [<<"h2">>, <<"http/1.1">>]}
    |TransOpts0],
    ranch:start_listener(Ref, ranch_ssl, TransOpts, cowboy_tls, ProtoOpts).

-spec connection_type(list()) -> {connection_type, worker | supervisor}.
connection_type(ProtoOpts) ->
    {_, Type} = maps:get(stream_handler, ProtoOpts, {cowboy_stream_h, supervisor}),
    {connection_type, Type}.


%% @todo Exclude platforms that do not support raw ipv6 socket options
-spec ipv6_supported() -> boolean().
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.
