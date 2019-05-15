%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2017 Marc Worrell

%% @doc Start/stop functions for Zotonic

%% Copyright 2009-2017 Marc Worrell
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
    run_tests/0,
    await_startup/0
]).

-compile([{parse_transform, lager_transform}]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(MIN_OTP_VERSION, "19").

%% @doc Start the zotonic server.
-spec start() -> ok.
start() ->
    test_erlang_version(),
    case zotonic_launcher_app:start() of
        ok -> ok;
        {error, Reason} ->
            lager:error("Zotonic start error: ~p~n", [Reason]),
            init:stop(1)
    end.

%% @doc Await till all parts are started - useful for testing
-spec await_startup() -> ok.
await_startup() ->
    zotonic_listen_http:await(),
    ok.

%% @doc Stop the zotonic server.
-spec stop() -> ok.
stop() ->
    application:stop(zotonic_launcher).


%% @doc Stop a zotonic server on a specific node
-spec stop([node()]) -> any().
stop([Node]) ->
    io:format("Stopping:~p~n", [Node]),
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

run_tests() ->
    z_media_preview_tests:test().

