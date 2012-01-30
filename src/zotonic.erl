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
-export([start/0, start/1, stop/0, stop/1, status/0, status/1, update/0, update/1, run_tests/0, ensure_started/1]).
-revision("$Id$").

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the zotonic server.
start() -> start([]).
	
%% @spec start(_Args) -> ok
%% @doc Start the zotonic server.
start(_Args) ->
    zotonic_deps:ensure(),    
    ensure_started(crypto),
    ensure_started(webzmachine),
    ensure_started(lager),
    ensure_started(mnesia),
    ok = application:start(zotonic).

%% @spec stop() -> ok
%% @doc Stop the zotonic server.
stop() ->
    Res = application:stop(zotonic),
    application:stop(mnesia),
    application:stop(lager),
    application:stop(webzmachine),
    application:stop(crypto),
    Res.


%% @spec stop([Node]) -> void()
%% @doc Stop a zotonic server on a specific node
stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    case net_adm:ping(Node) of
    	pong -> rpc:cast(Node, init, stop, []);
    	pang -> io:format("There is no node with this name~n")
    end,
    init:stop().


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

run_tests() ->
    z_media_preview_tests:test().
