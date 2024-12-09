%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2015-2024 Marc Worrell
%% @doc Check for changed files, notify the zotonic_filehandler of any changes
%% @end

%% Copyright 2015-2024 Marc Worrell
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

-module(zotonic_filewatcher_sup).
-author('Marc Worrell <marc@worrell.nl>').
-behaviour(supervisor).

%% External exports
-export([
    start_link/0,
    init/1,
    start_watchers/0,
    restart_watchers/0,
    is_watcher_running/0,

    watch_dirs/0,
    watch_dirs_expanded/0
    ]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/logger.hrl").


%% @doc API for starting the site supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Return the filewatcher gen_server(s) to be used.
init([]) ->
    Children = [
        {zotonic_filewatcher_handler,
          {zotonic_filewatcher_handler, start_link, []},
          permanent, 5000, worker, [zotonic_filewatcher_handler]}
    ],
    RestartStrategy = {one_for_all, 5, 10},
    {ok, {RestartStrategy, Children}}.

%% @doc Restart watchers because of a new application. This is because of new
%%      symlinks, the filewatcher_monitor resolves symlinks itself, so doesn't
%%      need to be restarted.
-spec restart_watchers() -> ok.
restart_watchers() ->
    case z_config:get(filewatcher_enabled) of
        true ->
            ?LOG_INFO(#{
                in => zotonic_filewatcher,
                text => <<"Restarting filewatchers">>
            }),
            zotonic_filewatcher_fswatch:restart(),
            zotonic_filewatcher_inotify:restart(),
            ok;
        false ->
            ok
    end.

%% @doc Check if inotify or fswatch are running and watching files. This is used
%% by the monitor to adjust its scanning frequency as the monitor is taking over
%% when inotify or fswatch are not able to run.
-spec is_watcher_running() -> boolean().
is_watcher_running() ->
    zotonic_filewatcher_fswatch:is_running()
    orelse zotonic_filewatcher_inotify:is_running().


start_watchers() ->
    case z_config:get(filewatcher_enabled) of
        true ->
            Children = watcher_children(z_config:get(filewatcher_enabled)),
            lists:foreach(
                fun(ChildSpec) ->
                    supervisor:start_child(?MODULE, ChildSpec)
                end,
                Children);
        false ->
            ok
    end.

watcher_children(true) ->
    Watchers = [
        zotonic_filewatcher_fswatch,
        zotonic_filewatcher_inotify
    ],
    which_watcher(Watchers) ++ monitor_watcher();
watcher_children(false) ->
    ?LOG_INFO(#{
        in => zotonic_filewatcher,
        text => <<"File watcher disabled in Zotonic config 'filewatcher_enabled'">>
    }),
    [
        {zotonic_filewatcher_beam_reloader,
          {zotonic_filewatcher_beam_reloader, start_link, [ false ]},
          permanent, 5000, worker, [zotonic_filewatcher_beam_reloader]}
    ].

monitor_watcher() ->
    % Start the filewatcher process and the beam reloader.
    % If the scanner is enabled then the beam reloader will tell the monitor which
    % directories need to be watched.
    [
        {zotonic_filewatcher_monitor,
          {zotonic_filewatcher_monitor, start_link, []},
          permanent, 5000, worker, [zotonic_filewatcher_monitor]},
        {zotonic_filewatcher_beam_reloader,
          {zotonic_filewatcher_beam_reloader, start_link, []},
          permanent, 5000, worker, [zotonic_filewatcher_beam_reloader]}
    ].

which_watcher([]) ->
    case z_config:get(filewatcher_scanner_enabled) of
        true ->
            ?LOG_WARNING(#{
                in => zotonic_filewatcher,
                text => <<"Please install fswatch or inotify-tools to improve automatic loading of changed files">>,
                filewatcher_scanner_enabled => true
            });
        false ->
            ?LOG_WARNING(#{
                in => zotonic_filewatcher,
                text => <<"Please install fswatch or inotify-tools to automatically load changed files">>,
                filewatcher_scanner_enabled => false
            })
    end,
    [];
which_watcher([M|Ms]) ->
    case M:is_installed() of
        true ->
            [
                {zotonic_filewatcher_beam_reloader,
                  {zotonic_filewatcher_beam_reloader, start_link, [false]},
                  permanent, 5000, worker, [zotonic_filewatcher_beam_reloader]},
                {M,
                  {M, start_link, []},
                  permanent, 5000, worker, [M]}
            ];
        false ->
            which_watcher(Ms)
    end.

%% @doc Return the list of all directories to watch
%% @todo Add a non recursive watch on zotonic_apps, _checkouts and the lib dir.
%%       To see if new applications are added (or removed).
-spec watch_dirs() -> list(string()).
watch_dirs() ->
    ZotonicDirs = [
        filename:join([ z_path:get_path(), "_checkouts" ]),
        build_lib_dir()
    ],
    lists:filter(fun(Dir) -> filelib:is_dir(Dir) end, ZotonicDirs).

%% @doc We expand all watch dirs, so that symbolic links to src, include, and priv are followed
-spec watch_dirs_expanded() -> list(string()).
watch_dirs_expanded() ->
    lists:foldl(
        fun(Dir, Acc) ->
            symlinks(Dir) ++ [ Dir | Acc ]
        end,
        [],
        watch_dirs()).

symlinks(Dir) ->
    All =  filelib:wildcard(filename:join([ Dir, "*" ]))
        ++ filelib:wildcard(filename:join([ Dir, "*", "{src,priv,include}" ])),
    lists:filter(
        fun(D) ->
            case filelib:is_file(D) of
                true ->
                    case file:read_link_info(D) of
                        {ok, #file_info{ type = symlink }} -> true;
                        _ -> false
                    end;
                false ->
                    false
            end
        end,
        All).

%% @doc Return the _build/default/lib directory
-spec build_lib_dir() -> file:filename().
build_lib_dir() ->
    filename:dirname(code:lib_dir(zotonic_filewatcher)).

