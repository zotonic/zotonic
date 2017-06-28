%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Zotonic Launcher, launches the Zotonic application server with
%%      the Zotonic Core, file watchers, and port listeners.

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

-module(zotonic_launcher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    write_pidfile(),
    zotonic_launcher_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    remove_pidfile(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_pidfile() -> file:filename().
get_pidfile() ->
    case os:getenv("ZOTONIC_PIDFILE") of
        false ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, "zotonic.pid");
        File ->
            File
    end.

-spec write_pidfile() -> ok.
write_pidfile() ->
    {ok, F} = file:open(get_pidfile(), [write]),
    ok = file:write(F, os:getpid()),
    ok = file:close(F).


-spec remove_pidfile() -> ok.
remove_pidfile() ->
    ok = file:delete(get_pidfile()).

