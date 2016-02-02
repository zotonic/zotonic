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

-include_lib("zotonic.hrl").

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
        {z_filewatcher_mtime,
          {z_filewatcher_mtime, start_link, [false]},
          permanent, 5000, worker, [z_filewatcher_mtime]},
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
        {z_filewatcher_mtime,
          {z_filewatcher_mtime, start_link, [IsScannerEnabled]},
          permanent, 5000, worker, [z_filewatcher_mtime]},
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
                {z_filewatcher_mtime,
                  {z_filewatcher_mtime, start_link, [true]},
                  permanent, 5000, worker, [z_filewatcher_mtime]},
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
%% @TODO: do not watch the 'files' directories of files.
-spec watch_dirs() -> list(string()).
watch_dirs() ->
    ZotonicDirs = [
        filename:join(os:getenv("ZOTONIC"), "src"),
        filename:join(os:getenv("ZOTONIC"), "modules"),
        filename:join(os:getenv("ZOTONIC"), "ebin"),

        filename:join(os:getenv("ZOTONIC"), "priv/sites"),
        filename:join(os:getenv("ZOTONIC"), "priv/modules"),

        z_path:user_sites_dir(),
        z_path:user_modules_dir()
    ],
    LinkedDirs = string:tokens(os:cmd("find " ++ z_utils:os_escape(os:getenv("ZOTONIC")) ++ " -type l"), "\n"),
    DepDirs = z_utils:wildcard(filename:join(os:getenv("ZOTONIC"), "deps/*/ebin")),
    ZotonicDirs ++ LinkedDirs ++ DepDirs.
