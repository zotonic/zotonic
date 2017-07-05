%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015 Marc Worrell
%% @doc Check for changed files, notify sites of any changes.

%% Copyright 2015 Marc Worrell
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

-module(z_filewatcher_sup).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(supervisor).

%% External exports
-export([
    start_link/0,
    init/1,
    watch_dirs/0
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc API for starting the site supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Return the filewatcher gen_server(s) to be used.
init([]) ->
    Children = children(z_config:get(filewatcher_enabled)),
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.


children(true) ->
    Watchers = [
        z_filewatcher_fswatch,
        z_filewatcher_inotify
    ],
    which_watcher(Watchers);
children(false) ->
    lager:error("FILEWATCHER: disabled"),
    [
        {z_code_reloader,
          {z_code_reloader, start_link, [false]},
          permanent, 5000, worker, [z_code_reloader]}
    ].

which_watcher([]) ->
    IsScannerEnabled = z_config:get(filewatcher_scanner_enabled, false),
    case IsScannerEnabled of
        true ->
            lager:error("FILEWATCHER: please install fswatch or inotify-tools to improve automatic loading of changed files");
        false ->
            lager:error("FILEWATCHER: please install fswatch or inotify-tools to automatically load changed files")
    end,
    [
        {z_filewatcher_monitor,
          {z_filewatcher_monitor, start_link, []},
          permanent, 5000, worker, [z_filewatcher_monitor]},
        {z_code_reloader,
          {z_code_reloader, start_link, [IsScannerEnabled]},
          permanent, 5000, worker, [z_code_reloader]}
    ];
which_watcher([M|Ms]) ->
    case M:is_installed() of
        true ->
            [
                {z_code_reloader,
                  {z_code_reloader, start_link, [false]},
                  permanent, 5000, worker, [z_code_reloader]},
                {M,
                  {M, start_link, []},
                  permanent, 5000, worker, [M]}
            ];
        false ->
            which_watcher(Ms)
    end.

%% @doc Return the list of all directories to watch
%% @todo do not watch the 'priv/files' directories
%% @todo check if need to follow softlinks in the _checkouts directory
-spec watch_dirs() -> list(string()).
watch_dirs() ->
    ZotonicDirs = [
        filename:join(z_path:get_path(), "apps"),
        filename:join(z_path:get_path(), "_checkouts"),
        filename:join(z_path:get_path(), "priv/translations"),
        z_path:build_lib_dir()

        % These we have to scan to pick up any new apps, so that they can be linked in _checkouts
        % Actually, we might hold off of scanning, as _checkouts will need to change (and the compiled
        % files if rebar3 has been run)
        %
        % z_path:user_sites_dir(),
        % z_path:user_modules_dir()
    ],
    lists:filter(fun(Dir) -> filelib:is_dir(Dir) end, ZotonicDirs).
