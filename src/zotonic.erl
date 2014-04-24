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

-compile([{parse_transform, lager_transform}]).

-define(MIN_OTP_VERSION, "15B03"). %% note -- *without* the initial R (since OTP 17.0 the R is dropped)


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {not_started, Dep}} ->
            case ensure_started(Dep) of
                ok ->
                    ensure_started(App);
                Error ->
                    Error
            end;
        {error, {already_started, App}} ->
            ok;
        {error, {Tag, Msg}} when is_list(Tag), is_list(Msg) ->
                io_lib:format("~s: ~s", [Tag, Msg]);
        {error, {bad_return, {{M, F, Args}, Return}}} ->
                A = string:join([io_lib:format("~p", [A])|| A <- Args], ", "),
                io_lib:format("~s failed to start due to a bad return value from call ~s:~s(~s):~n~p", [App, M, F, A, Return]);
        {error, Reason} ->
            io_lib:format("~p", [Reason])
    end.

%% @spec start() -> ok
%% @doc Start the zotonic server.
start() -> start([]).
	
%% @spec start(_Args) -> ok
%% @doc Start the zotonic server.
start(_Args) ->
    test_erlang_version(),
    zotonic_deps:ensure(),    
    case ensure_started(zotonic) of
        ok -> ok;
	Message ->
            lager:error("Zotonic start error: ~s~n", [Message]),
            init:stop()
    end.

%% @spec stop() -> ok
%% @doc Stop the zotonic server.
stop() ->
    Res = application:stop(zotonic),
    application:stop(emqtt),
    application:stop(eiconv),
    application:stop(mnesia),
    application:stop(lager),
    application:stop(webzmachine),
    application:stop(crypto),
    Res.


%% @spec stop([Node]) -> void()
%% @doc Stop a zotonic server on a specific node
stop([Node]) ->
    io:format("Stopping:~p~n",[Node]),
    case net_adm:ping(Node) of
    	pong -> rpc:cast(Node, init, stop, []);
    	pang -> io:format("There is no node with this name~n")
    end,
    init:stop().

%% @doc Just returns 'pong'; used by shell scripts to determine if node is alive.
ping() ->
    pong.

%% @spec status() -> ok
%% @doc Print the status of the current node.
status() ->
    status([node()]).

%% @spec status([node()]) -> ok
%% @doc Get server status.  Prints the state of sites running.
status([Node]) ->
	[io:format("~-20s- ~s~n", [Site, Status]) || [Site,Status|_] <- rpc:call(Node, z_sites_manager, get_sites_status, [])],
	ok.

%% @spec update() -> ok
%% @doc Update the server.  Compiles and loads any new code, flushes caches and rescans all modules.
update() ->
    z:m(),
    ok.


%% @spec update([Node]) -> ok
%% @doc Update the server on a specific node with new code on disk and flush the caches.
update([Node]) ->
    io:format("Update:~p~n",[Node]),
    case net_adm:ping(Node) of
    	pong -> rpc:cast(Node, zotonic, update, []);
    	pang -> io:format("There is no node with this name~n")
    end,
    init:stop().


test_erlang_version() ->
    case otp_version() of
        Version when Version < ?MIN_OTP_VERSION ->
            io:format("Zotonic needs at least Erlang release ~p; this is ~p~n", [?MIN_OTP_VERSION, erlang:system_info(otp_release)]),
            erlang:exit({minimal_otp_version, ?MIN_OTP_VERSION});
        _ ->
            ok
    end.

%% @doc Strip the optional "R" from the OTP release because from 17.0 onwards it is unused
otp_version() ->
    case erlang:system_info(otp_release) of
        [$R | V] -> V;
        V -> V
    end.

run_tests() ->
    z_media_preview_tests:test().
