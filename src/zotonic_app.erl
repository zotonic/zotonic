%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Callbacks for the zotonic application.

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

-module(zotonic_app).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(application).
-export([start/2, stop/1, get_path/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for zotonic.
start(_Type, _StartArgs) ->
    write_pidfile(),
	set_path(),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
	ensure_started(inets),
	inets:start(httpc,[{profile,zotonic}]),
    zotonic_deps:ensure(),
    zotonic_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for zotonic.
stop(_State) ->
    remove_pidfile(),
    ok.

set_path() ->
	P = code:all_loaded(),
	Path = filename:dirname(filename:dirname(proplists:get_value(?MODULE, P))),
	application:set_env(zotonic, lib_dir, Path).

get_path() ->
	application:get_env(zotonic, lib_dir).


%% Pid-file handling

get_pidfile() ->
    case os:getenv("ZOTONIC_PIDFILE") of 
        false -> {ok, Cwd} = file:get_cwd(),
                 filename:join(Cwd, "zotonic.pid");
        File -> File
    end.

write_pidfile() ->
    {ok, F} = file:open(get_pidfile(), [write]),
    ok = file:write(F, os:getpid()),
    ok = file:close(F).

remove_pidfile() ->
    ok = file:delete(get_pidfile()).
