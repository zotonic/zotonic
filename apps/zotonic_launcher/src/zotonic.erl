%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2022 Marc Worrell

%% @doc Start/stop functions for Zotonic

%% Copyright 2009-2022 Marc Worrell
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

-export([
    start/0,
    stop/0,
    stop/1,
    ping/0,
    status/0,
    status/1,
    update/0,
    update/1,
    runtests/1,
    await_startup/0,
    await_startup/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MIN_OTP_VERSION, "23").

%% @doc Start the zotonic server.
-spec start() -> ok.
start() ->
    test_erlang_version(),
    case zotonic_launcher_app:start() of
        ok -> ok;
        {error, Reason} ->
            ?LOG_EMERGENCY(#{
                text => <<"Zotonic start error">>,
                in => zotonic_launcher,
                result => error,
                reason => Reason
            }),
            init:stop(1)
    end.

%% @doc Await till all parts are started - useful for testing
-spec await_startup() -> ok.
await_startup() ->
    zotonic_listen_http:await(),
    ok.

-spec await_startup( atom() ) -> ok | {error, timeout | stopped | removing | term()}.
await_startup(Site) ->
    ok = await_startup(),
    await_startup_1(Site, 500).

await_startup_1(_Site, 0) ->
    {error, timeout};
await_startup_1(Site, N) ->
    case z_sites_manager:wait_for_running(Site) of
        {error, bad_name} ->
            timer:sleep(100),
            await_startup_1(Site, N-1);
        {error, _} = Error ->
            Error;
        ok ->
            ok
    end.

%% @doc Called by the 'make test' commands.
-spec runtests( list(atom()) ) -> ok.
runtests(Tests) ->
    erlang:spawn(
        fun() ->
            io:format("~nRunning tests:"),
            lists:foreach(
                fun(T) ->
                    io:format("~n - ~p", [T])
                end,
                Tests),
            io:format("~nWaiting for zotonic_site_testsandbox to be started...~n"),
            ok = await_startup(zotonic_site_testsandbox),
            io:format("~nGive system some time to stabilize...~n"),
            timer:sleep(5000),
            io:format("~nStarting eunit tests~n"),
            z_memo:disable(),
            % z_memo:enable(),
            case eunit:test(Tests, []) of
                ok -> 
                    erlang:halt(0);
                error ->
                    erlang:halt(1)
            end
        end),
    ok.

%% @doc Stop all sites, the zotonic server and the beam.
-spec stop() -> ok.
stop() ->
    Sites = z_sites_manager:get_sites(),
    maps:fold(
        fun
            (_Site, stopping, _Acc) -> ok;
            (_Site, stopped, _Acc) -> ok;
            (Site, _Status, _Acc) -> z_sites_manager:stop(Site)
        end,
        ok,
        Sites),
    % Wait a bit till all sites are stopped (max 5 secs)
    await_sites_stopping(50),
    application:stop(zotonic_launcher),
    application:stop(exometer),
    application:stop(jobs),
    application:stop(mnesia),
    application:stop(epgsql),
    heart:set_cmd("echo ok"),
    erlang:halt(0).


await_sites_stopping(0) -> ok;
await_sites_stopping(N) ->
    case z_sites_manager:is_sites_running() of
        true ->
            timer:sleep(100),
            await_sites_stopping(N-1);
        false ->
            ok
    end.


%% @doc Stop a zotonic server on a specific node
-spec stop([node()]) -> any().
stop([Node]) ->
    io:format("Stopping:~p~n", [Node]),
    case net_adm:ping(Node) of
        pong -> rpc:cast(Node, zotonic, stop, []);
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
    [io:format(
        "~-20s- ~s~n",
        [Site, Status]
    ) || [Site, Status | _] <- rpc:call(Node, z_sites_manager, get_sites_status, [])],
    ok.

%% @doc Update the server. Compiles and loads any new code, flushes caches and rescans all modules.
-spec update() -> ok.
update() ->
    z:m(),
    ok.

%% @doc Update the server on a specific node with new code on disk and flush the caches.
-spec update([node()]) -> ok.
update([Node]) ->
    io:format("Update:~p~n", [Node]),
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
            io:format(
                "Zotonic needs at least Erlang release ~p; this is ~p~n",
                [?MIN_OTP_VERSION, erlang:system_info(otp_release)]
            ),
            erlang:exit({minimal_otp_version, ?MIN_OTP_VERSION});
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
