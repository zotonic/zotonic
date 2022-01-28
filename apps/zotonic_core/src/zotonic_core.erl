%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2021 Marc Worrell
%% @doc Zotonic core - main routines to 'reason' about the current Zotonic
%%      installation.

%% Copyright 2017-2021 Marc Worrell
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

-module(zotonic_core).

-author('Marc Worrell <marc@worrell.nl>').

-include_lib("kernel/include/logger.hrl").

-export([
    is_zotonic_project/0,
    is_testsandbox/0,
    is_app_available/1,

    setup/1
    ]).

%% @doc Check if the current site is running the testsandbox
-spec is_testsandbox() -> boolean().
is_testsandbox() ->
    case atom_to_list(node()) of
        "zotonic001_testsandbox@" ++ _ -> true;
        _ -> false
    end.

%% @doc Check if this running Zotonic is the main Git project.
%%      This is used for e.g. the .pot generation.
is_zotonic_project() ->
    is_app_available(zotonic)
    andalso is_app_available(zotonic_core).

is_app_available(App) ->
    case code:which(App) of
        non_existing -> false;
        Path when is_list(Path) -> true
    end.

%% @doc Initial setup before starting Zotonic, after config files are loaded. This
%% is normally called by the zotonic_launcher_app, which also reads all config files.
-spec setup( node() ) -> ok.
setup(Node) ->
    io:setopts([{encoding, unicode}]),
    assert_schedulers( erlang:system_info(schedulers) ),
    load_applications(),
    set_configs(),
    case node() of
        Node -> ensure_mnesia_schema();
        _ -> ok
    end.

%% @doc Load the applications so that their settings are also loaded.
load_applications() ->
    application:load(setup),
    application:load(mnesia),
    application:load(filezcache),
    application:load(zotonic_core).

set_configs() ->
    application:set_env(setup, log_dir, z_config:get(log_dir)),
    application:set_env(setup, data_dir, z_config:get(data_dir)),
    % Lager should log in the log_dir
    application:set_env(lager, log_root, z_config:get(log_dir)),
    % Store filezcache data in the cache_dir.
    FileZCache = filename:join([ z_config:get(cache_dir), "filezcache", atom_to_list(node()) ]),
    application:set_env(filezcache, data_dir, filename:join([ FileZCache, "data" ])),
    application:set_env(filezcache, journal_dir, filename:join([ FileZCache, "journal" ])).

assert_schedulers(1) ->
    io:format("FATAL: Not enough schedulers, please start with 2 or more schedulers.~nUse: ERLOPTS=\"+S 4:4\" ./bin/zotonic debug~n~n"),
    erlang:halt();
assert_schedulers(_N) ->
    ok.


%% @doc Ensure that mnesia has created its schema in the configured data/mnesia directory.
-spec ensure_mnesia_schema() -> ok.
ensure_mnesia_schema() ->
    case mnesia_dir() of
        {ok, Dir} ->
            case filelib:is_dir(Dir) andalso filelib:is_regular(filename:join(Dir, "schema.DAT")) of
                true -> ok;
                false -> ok = mnesia:create_schema([node()])
            end;
        undefined ->
            ?LOG_NOTICE("No mnesia directory defined, running without persistent email queue and filezcache. "
                        "To enable persistency, add to erlang.config: {mnesia,[{dir,\"data/mnesia\"}]}"),
            ok
    end.

mnesia_dir() ->
    case is_testsandbox() of
        true ->
            application:unset_env(mnesia, dir),
            undefined;
        false ->
            mnesia_dir_config()
    end.

mnesia_dir_config() ->
    case application:get_env(mnesia, dir) of
        {ok, none} -> undefined;
        {ok, ""} -> undefined;
        {ok, setup} -> mnesia_data_dir();
        {ok, "data/mnesia"} -> mnesia_data_dir();
        {ok, "priv/mnesia"} -> mnesia_data_dir();
        {ok, Dir} -> {ok, Dir};
        undefined -> mnesia_data_dir()
    end.

mnesia_data_dir() ->
    MnesiaDir = filename:join([ z_config:get(data_dir), atom_to_list(node()), "mnesia" ]),
    case z_filelib:ensure_dir(MnesiaDir) of
        ok ->
            application:set_env(mnesia, dir, MnesiaDir),
            {ok, MnesiaDir};
        {error, _} = Error ->
            ?LOG_ERROR("Could not create mnesia dir \"~s\": ~p",
                        [MnesiaDir, Error]),
            undefined
    end.
