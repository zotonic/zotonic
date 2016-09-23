%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Start/stop functions for Zotonic

%% Copyright 2009 Marc Worrell
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

-module(zotonic).
-author('Marc Worrell <marc@worrell.nl>').

-export([start/0, start/1, stop/0, stop/1, ping/0, status/0, status/1, update/0, update/1, run_tests/0, ensure_started/1]).
-export([sni_fun/1]).

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic.hrl").

-define(MIN_OTP_VERSION, "18"). %% note -- *without* the initial R (since OTP 17.0 the R is dropped)

-spec ensure_started(atom()) -> ok | {error, term()}.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            case ensure_started(Dep) of
                ok -> ensure_started(App);
                {error, _} = Error -> Error
            end;
        {error, {already_started, App}} ->
            ok;
        {error, {Tag, Msg}} when is_list(Tag), is_list(Msg) ->
            {error, lists:flatten(io_lib:format("~s: ~s", [Tag, Msg]))};
        {error, {bad_return, {{M, F, Args}, Return}}} ->
            A = string:join([io_lib:format("~p", [A])|| A <- Args], ", "),
            {error, lists:flatten(io_lib:format("~s failed to start due to a bad return value from call ~s:~s(~s):~n~p", [App, M, F, A, Return]))};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start the zotonic server.
-spec start() -> ok.
start() -> start([]).

%% @doc Start the zotonic server.
-spec start(list()) -> ok.
start(_Args) ->
    test_erlang_version(),
    ensure_mnesia_schema(),
    case ensure_started(zotonic) of
        ok ->
            start_http_listeners();
        {error, Reason} ->
            lager:error("Zotonic start error: ~p~n", [Reason]),
            init:stop(1)
    end.

%% @doc Stop the zotonic server.
-spec stop() -> ok.
stop() ->
    stop_http_listeners(),
    Res = application:stop(zotonic),
    application:stop(emqtt),
    Res.


%% @doc Stop a zotonic server on a specific node
-spec stop([node()]) -> any().
stop([Node]) ->
    io:format("Stopping:~p~n",[Node]),
    case net_adm:ping(Node) of
        pong -> rpc:cast(Node, init, stop, []);
        pang -> io:format("There is no node with this name~n")
    end,
    init:stop().

%% @doc Just returns 'pong'; used by shell scripts to determine if node is alive.
-spec ping() -> pong.
ping() ->
    pong.

%% @doc Print the status of the current node.
-spec status() -> ok.
status() ->
    status([node()]).

%% @doc Get server status.  Prints the state of sites running.
-spec status([node()]) -> ok.
status([Node]) ->
    [io:format("~-20s- ~s~n", [Site, Status]) || [Site,Status|_] <- rpc:call(Node, z_sites_manager, get_sites_status, [])],
    ok.

%% @doc Update the server.  Compiles and loads any new code, flushes caches and rescans all modules.
-spec update() -> ok.
update() ->
    z:m(),
    ok.

%% @doc Stop all HTTP listeners
-spec stop_http_listeners() -> ok.
stop_http_listeners() ->
    lists:foreach(
            fun cowboy:stop_listener/1,
            [ zotonic_http_listener_ipv4,
              zotonic_https_listener_ipv4,
              zotonic_http_listener_ipv6,
              zotonic_https_listener_ipv6]).

%% @doc Start the HTTP listeners
-spec start_http_listeners() -> ok.
start_http_listeners() ->
    application:set_env(cowmachine, server_header, <<"Zotonic/", (z_convert:to_binary(?ZOTONIC_VERSION))/binary>>),
    WebIp = z_config:get(listen_ip),
    WebPort = z_config:get(listen_port),
    SSLPort = z_config:get(ssl_listen_port),
    CowboyOpts = #{
        middlewares => [ z_sites_dispatcher, z_cowmachine_middleware ],
        env => #{}
    },

    lager:info("Web server listening on IPv4 ~p:~p, SSL ~p::~p", [WebIp, WebPort, WebIp, SSLPort]),
    {ok, _} = cowboy:start_clear(
        zotonic_http_listener_ipv4,
        z_config:get(inet_acceptor_pool_size),
        [   inet,
            {port, WebPort},
            {backlog, z_config:get(inet_backlog)}
            | case WebIp of any -> []; _ -> [{ip, WebIp}] end
        ],
        CowboyOpts),
    case SSLPort of
        none ->
            ok;
        _ ->
            {ok, _} = cowboy:start_tls(
                zotonic_https_listener_ipv4,
                z_config:get(ssl_acceptor_pool_size),
                [   inet,
                    {port, SSLPort},
                    {backlog, z_config:get(ssl_backlog)},
                    {certfile, "via_sni_fun"},
                    {sni_fun, fun ?MODULE:sni_fun/1}
                    | case WebIp of any -> []; _ -> [{ip, WebIp}] end
                ],
                CowboyOpts)
    end,
    case {WebIp, ipv6_supported()} of
        {any, true} ->
            lager:info("Web server listening on IPv6 ::~p, SSL ::~p", [WebPort, SSLPort]),
            {ok, _} = cowboy:start_clear(
                zotonic_http_listener_ipv6,
                z_config:get(inet_acceptor_pool_size),
                [   inet6,
                    {ipv6_v6only, true},
                    {port, WebPort},
                    {backlog, z_config:get(inet_backlog)}
                ],
                CowboyOpts),

            case SSLPort of
                none ->
                    ok;
                _ ->
                    {ok, _} = cowboy:start_tls(
                        zotonic_https_listener_ipv6,
                        z_config:get(ssl_acceptor_pool_size),
                        [   inet6,
                            {ipv6_v6only, true},
                            {port, SSLPort},
                            {backlog, z_config:get(ssl_backlog)},
                            {certfile, "via_sni_fun"},
                            {sni_fun, fun ?MODULE:sni_fun/1}
                        ],
                        CowboyOpts),
                    ok
            end;
        _ ->
            ok
    end.

%% @doc Let sites return their own keys and certificates.
-spec sni_fun(string()) -> [ssl:ssl_option()] | undefined.
sni_fun(Hostname) ->
    HostnameBin = z_convert:to_binary(Hostname),
    case z_sites_dispatcher:get_site_for_hostname(HostnameBin) of
        undefined -> undefined;
        {ok, Host} ->
            z_notifier:first(#ssl_options{server_name=HostnameBin}, z_context:new(Host))
    end.

%% @todo Exclude platforms that do not support raw ipv6 socket options
-spec ipv6_supported() -> boolean().
ipv6_supported() ->
    case (catch inet:getaddr("localhost", inet6)) of
        {ok, _Addr} -> true;
        {error, _} -> false
    end.


%% @doc Ensure that mnesia has created its schema in the configured mnesia directory.
-spec ensure_mnesia_schema() -> ok.
ensure_mnesia_schema() ->
    application:load(mnesia),
    case application:get_env(mnesia, dir) of
        undefined ->
            lager:info("No mnesia directory defined, running without persistent email queue and filezcache.~n"
                       "To enable persistency, add to erlang.config: {mnesia,[{dir,\"priv/mnesia\"}]}~n~n"),
            ok;
        {ok, Dir} ->
            case filelib:is_dir(Dir) andalso filelib:is_regular(filename:join(Dir,"schema.DAT")) of
                true ->
                    ok;
                false ->
                    ok = mnesia:create_schema([node()])
            end
    end.

%% @doc Update the server on a specific node with new code on disk and flush the caches.
-spec update([node()]) -> ok.
update([Node]) ->
    io:format("Update:~p~n",[Node]),
    case net_adm:ping(Node) of
        pong -> rpc:cast(Node, zotonic, update, []);
        pang -> io:format("There is no node with this name~n")
    end,
    init:stop().

-spec test_erlang_version() -> ok.
test_erlang_version() ->
    % Check for minimal OTP version
    case otp_release() of
        Version when Version < ?MIN_OTP_VERSION ->
            io:format("Zotonic needs at least Erlang release ~p; this is ~p~n", [?MIN_OTP_VERSION, erlang:system_info(otp_release)]),
            erlang:exit({minimal_otp_version, ?MIN_OTP_VERSION});
        _ ->
            ok
    end,
    % Check on problematic releases
    case otp_version() of
        "18.3.2" ->
            io:format("Erlang version 18.3.2 has a problem with SSL and ranch, please upgrade your Erlang version to 18.3.3 or later~n"),
            erlang:exit({broken_otp_version, "18.3.2"});
        _ ->
            ok
    end.

%% @doc Strip the optional "R" from the OTP release because from 17.0 onwards it is unused
-spec otp_release() -> string().
otp_release() ->
    case erlang:system_info(otp_release) of
        [$R | V] -> V;
        V -> V
    end.

%% @doc Return the specific otp version, 
-spec otp_version() -> string().
otp_version() ->
    case file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])) of
        {ok, Version} -> binary_to_list(z_string:trim(Version));
        {error, _} -> erlang:system_info(otp_release)
    end.


run_tests() ->
    z_media_preview_tests:test().

