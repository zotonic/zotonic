%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2024 Marc Worrell
%% @doc Generic support for finding and parsing config files.
%% @end

%% Copyright 2019-2024 Marc Worrell
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

-module(z_config_files).

-export([
    zotonic_config_files/0,
    zotonic_config_files/1,

    security_dir/0,
    log_dir/0,
    data_dir/0,
    cache_dir/0,
    config_dir/0,
    config_dir/1,
    files/1,
    files/2,
    consult/1
]).

-include_lib("zotonic_core/include/zotonic_release.hrl").
-include_lib("yamerl/include/yamerl_errors.hrl").
-include_lib("kernel/include/logger.hrl").

%% @doc List all zotonic config files in the zotonic config directory
%%      and the "config.d" subdirectory for the current node.
%%      Zotonic config files are "zotonic.*" files in the root of the
%%      config directory and all files the 'config.d' subdirectory.
-spec zotonic_config_files() -> list( file:filename() ).
zotonic_config_files() ->
    zotonic_config_files(node()).

%% @doc List all zotonic config files in the zotonic config directory
%%      and the "config.d" subdirectory.
%%      Zotonic config files are "zotonic.*" files in the root of the
%%      config directory and all files the 'config.d' subdirectory.
-spec zotonic_config_files( node() ) -> list( file:filename() ).
zotonic_config_files(Node) ->
    case config_dir(Node) of
        {ok, Dir} ->
            Files = lists:filter(fun(F) -> not is_erlang_config(F) end, files(Dir)),
            SubFiles= files( filename:join([ Dir, "config.d" ]) ),
            Files ++ SubFiles;
        {error, _} ->
            []
    end.

is_erlang_config(F) ->
    case filename:basename(F) of
        "zotonic" -> false;
        "zotonic." ++ _ -> false;
        _ -> filename:extension(F) =:= ".config"
    end.

%% @doc Find the default directory for certificates and other secrets.
%% Checks the following locations:
%%
%% <ol>
%%   <li>The environment variable <tt>ZOTONIC_SECURITY_DIR</tt></li>
%%   <li>The directory <tt>$HOME/.zotonic/security</tt></li>
%%   <li>The directory <tt>/etc/zotonic/security</tt>  (only on Unix)</li>
%%   <li>The OS specific directory for application config files</li>
%% </ol>
%%
%% If no directory is found then the OS specific directory with the
%% the subdirectory <tt>security</tt> is used:
%%
%% <ol>
%%   <li>Linux: <tt>$HOME/.config/zotonic/security/</tt></li>
%%   <li>macOS: <tt>$HOME/Library/Application Support/zotonic/security/</tt></li>
%% </ol>
%%
-spec security_dir() -> {ok, file:filename_all()} | {error, term()}.
security_dir() ->
    case os:getenv("ZOTONIC_SECURITY_DIR") of
        false ->
            security_dir_1();
        "" ->
            security_dir_1();
        Dir ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

security_dir_1() ->
    HomeLocs = case os:getenv("HOME") of
        false -> [];
        "" -> [];
        Home ->
            [
                filename:join([Home, ".zotonic", "security"])
            ]
    end,
    EtcLocs = case os:type() of
        {unix, _} ->
            [
                filename:join(["/etc/zotonic", "security"])
            ];
        {_, _} ->
            []
    end,
    SystemConfigDir = filename:basedir(user_config, "zotonic"),
    SystemLocs = [
        filename:join([SystemConfigDir, "security" ])
    ],
    Locs = HomeLocs ++ EtcLocs ++ SystemLocs,
    case lists:dropwhile(fun(D) -> not filelib:is_dir(D) end, Locs) of
        [] ->
            % Use the OS specific default
            SecurityDir = filename:join([SystemConfigDir, "security"]),
            % The '$HOME/.config' dir is not pre-created on some Linux VMs
            _ = z_filelib:ensure_dir(SystemConfigDir),
            case file:make_dir(SystemConfigDir) of
                ok -> file:change_mode(SystemConfigDir, 8#00700);
                {error, _} -> ok
            end,
            case file:make_dir(SecurityDir) of
                ok -> file:change_mode(SecurityDir, 8#00700);
                {error, _} -> ok
            end,
            case filelib:is_dir(SecurityDir) of
                true ->
                    ?LOG_INFO(#{
                        text => <<"Created security directory">>,
                        in => zotonic_core,
                        path => SecurityDir
                    }),
                    {ok, SecurityDir};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Could not create security directory">>,
                        in => zotonic_core,
                        path => SecurityDir
                    }),
                    {error, enoent}
            end;
        [ D | _ ] ->
            {ok, D}
    end.

%% @doc Find the default directory for log files.
%% Checks the following locations:
%%
%% <ol>
%%   <li>The environment variable <tt>ZOTONIC_LOG_DIR</tt></li>
%%   <li>Local working directory <tt>logs</tt></li>
%%   <li>The OS specific directory for application log files</li>
%% </ol>
%%
%% If no directory is found then the OS specific directory is used:
%%
%% <ol>
%%   <li>Linux: <tt>$HOME/.cache/zotonic/log/</tt></li>
%%   <li>macOS: <tt>$HOME/Library/Logs/zotonic//</tt></li>
%% </ol>
%%
-spec log_dir() -> {ok, file:filename_all()} | {error, term()}.
log_dir() ->
    case os:getenv("ZOTONIC_LOG_DIR") of
        false ->
            logs_dir_1();
        "" ->
            logs_dir_1();
        Dir ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

logs_dir_1() ->
    HomeLocs = [
        "logs"
    ],
    SystemLogDir = filename:basedir(user_log, "zotonic"),
    SystemLocs = [
        SystemLogDir
    ],
    Locs = HomeLocs ++ SystemLocs,
    case lists:dropwhile(fun(D) -> not filelib:is_dir(D) end, Locs) of
        [] ->
            % Use the OS specific default
            % The '$HOME/.config' dir is not pre-created on some Linux VMs
            _ = z_filelib:ensure_dir(SystemLogDir),
            case file:make_dir(SystemLogDir) of
                ok -> file:change_mode(SystemLogDir, 8#00700);
                {error, _} -> ok
            end,
            case filelib:is_dir(SystemLogDir) of
                true ->
                    ?LOG_INFO(#{
                        text => <<"Create log directory">>,
                        in => zotonic_core,
                        path => SystemLogDir
                    }),
                    {ok, SystemLogDir};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Could not create log directory">>,
                        in => zotonic_core,
                        path => SystemLogDir
                    }),
                    {error, enoent}
            end;
        [ D | _ ] ->
            {ok, D}
    end.

%% @doc Find the default directory for data files.
%% Checks the following locations:
%%
%% <ol>
%%   <li>The environment variable <tt>ZOTONIC_DATA_DIR</tt></li>
%%   <li>Local working directory <tt>data</tt></li>
%%   <li>The OS specific directory for application data files</li>
%% </ol>
%%
%% If no directory is found then the OS specific directory is used:
%%
%% <ol>
%%   <li>Linux: <tt>$HOME/.local/share/zotonic/</tt></li>
%%   <li>macOS: <tt>$HOME/Library/Application Support/zotonic/</tt></li>
%% </ol>
%%
-spec data_dir() -> {ok, file:filename_all()} | {error, term()}.
data_dir() ->
    case os:getenv("ZOTONIC_DATA_DIR") of
        false ->
            data_dir_1();
        "" ->
            data_dir_1();
        Dir ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

data_dir_1() ->
    HomeLocs = [
        "data"
    ],
    SystemDataDir = filename:basedir(user_data, "zotonic"),
    SystemLocs = [
        SystemDataDir
    ],
    Locs = HomeLocs ++ SystemLocs,
    case lists:dropwhile(fun(D) -> not filelib:is_dir(D) end, Locs) of
        [] ->
            % Use the OS specific default
            % The '$HOME/.config' dir is not pre-created on some Linux VMs
            _ = z_filelib:ensure_dir(SystemDataDir),
            case file:make_dir(SystemDataDir) of
                ok -> file:change_mode(SystemDataDir, 8#00700);
                {error, _} -> ok
            end,
            case filelib:is_dir(SystemDataDir) of
                true ->
                    ?LOG_INFO(#{
                        text => <<"Created data directory">>,
                        in => zotonic_core,
                        path => SystemDataDir
                    }),
                    {ok, SystemDataDir};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Could not create data directory">>,
                        in => zotonic_core,
                        path => SystemDataDir
                    }),
                    {error, enoent}
            end;
        [ D | _ ] ->
            {ok, D}
    end.


%% @doc Find the default directory for cache files.
%% Checks the following locations:
%%
%% <ol>
%%   <li>The environment variable <tt>ZOTONIC_CACHE_DIR</tt></li>
%%   <li>Local working directory <tt>caches</tt></li>
%%   <li>The OS specific directory for application cache files</li>
%% </ol>
%%
%% If no directory is found then the OS specific directory is used:
%%
%% <ol>
%%   <li>Linux: <tt>$HOME/.cache/zotonic/</tt></li>
%%   <li>macOS: <tt>$HOME/Library/Caches/zotonic/</tt></li>
%% </ol>
%%
-spec cache_dir() -> {ok, file:filename_all()} | {error, term()}.
cache_dir() ->
    case os:getenv("ZOTONIC_LOG_DIR") of
        false ->
            cache_dir_1();
        "" ->
            cache_dir_1();
        Dir ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

cache_dir_1() ->
    HomeLocs = [
        "caches"
    ],
    SystemCacheDir = filename:basedir(user_cache, "zotonic"),
    SystemLocs = [
        SystemCacheDir
    ],
    Locs = HomeLocs ++ SystemLocs,
    case lists:dropwhile(fun(D) -> not filelib:is_dir(D) end, Locs) of
        [] ->
            % Use the OS specific default
            % The '$HOME/.config' dir is not pre-created on some Linux VMs
            _ = z_filelib:ensure_dir(SystemCacheDir),
            case file:make_dir(SystemCacheDir) of
                ok -> file:change_mode(SystemCacheDir, 8#00700);
                {error, _} -> ok
            end,
            case filelib:is_dir(SystemCacheDir) of
                true ->
                    ?LOG_INFO(#{
                        text => <<"Created cache directory">>,
                        in => zotonic_core,
                        path => SystemCacheDir
                    }),
                    {ok, SystemCacheDir};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Could not create cache directory">>,
                        in => zotonic_core,
                        path => SystemCacheDir
                    }),
                    {error, enoent}
            end;
        [ D | _ ] ->
            {ok, D}
    end.

%% @doc Find the directory with the configuration files. Defaults to the
%% OS specific directory for all configurations. This checks a list
%% of possible locations:
%%
%% <ol>
%%   <li>The init argument <tt>zotonic_config_dir</tt></li>
%%   <li>The environment variable <tt>ZOTONIC_CONFIG_DIR</tt></li>
%%   <li>The directory <tt>$HOME/.zotonic</tt></li>
%%   <li>The directory <tt>/etc/zotonic</tt>  (only on Unix)</li>
%%   <li>The OS specific directory for application config files</li>
%% </ol>
%%
%% In the last three cases subdirectories are also checked, in
%% the following order:
%%
%% <ol>
%%   <li>The complete Erlang node name</li>
%%   <li>The short node name without the server address</li>
%%   <li>The complete Zotonic version (eg. 1.2.3)</li>
%%   <li>The minor Zotonic version (eg. 1.2)</li>
%%   <li>The major Zotonic version (eg. 1)</li>
%%   <li>The directory itself, without any version</li>
%% </ol>
%%
%% If no directory is found then the OS specific directory with the
%% the major Zotonic version is used. Examples:
%%
%% <ol>
%%   <li>Linux: <tt>$HOME/.config/zotonic/config/1/</tt></li>
%%   <li>macOS: <tt>$HOME/Library/Application Support/zotonic/config/1/</tt></li>
%% </ol>
%%
-spec config_dir() -> {ok, file:filename_all()} | {error, term()}.
config_dir() ->
    config_dir( node() ).

-spec config_dir( node() ) -> {ok, file:filename_all()} | {error, term()}.
config_dir(Node) ->
    case proplists:get_value(zotonic_config_dir, init:get_arguments()) of
        undefined ->
            config_dir_env(Node);
        [] ->
            config_dir_env(Node);
        [ Dir ] ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

config_dir_env(Node) ->
    case os:getenv("ZOTONIC_CONFIG_DIR") of
        false ->
            config_dir_find(Node);
        "" ->
            config_dir_find(Node);
        Dir ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

config_dir_find(Node) ->
    {MajorVersion, MinorVersion} = split_version(?ZOTONIC_VERSION),
    HomeLocs = case os:getenv("HOME") of
        false -> [];
        "" -> [];
        Home ->
            [
                filename:join([Home, ".zotonic", atom_to_list(Node)]),
                filename:join([Home, ".zotonic", base_nodename(Node)]),
                filename:join([Home, ".zotonic", ?ZOTONIC_VERSION]),
                filename:join([Home, ".zotonic", MinorVersion]),
                filename:join([Home, ".zotonic", MajorVersion]),
                filename:join([Home, ".zotonic"])
            ]
    end,
    EtcLocs = case os:type() of
        {unix, _} ->
            [
                filename:join(["/etc/zotonic", atom_to_list(Node)]),
                filename:join(["/etc/zotonic", base_nodename(Node)]),
                filename:join(["/etc/zotonic", ?ZOTONIC_VERSION]),
                filename:join(["/etc/zotonic", MinorVersion]),
                filename:join(["/etc/zotonic", MajorVersion]),
                filename:join(["/etc/zotonic"])
            ];
        {_, _} ->
            []
    end,
    SystemConfigDir = filename:basedir(user_config, "zotonic"),
    % Use 'config' subdir as on macOS the user_config and user_data
    % directories are the same.
    SystemLocs = [
        filename:join([SystemConfigDir, "config", atom_to_list(Node)]),
        filename:join([SystemConfigDir, "config", base_nodename(Node)]),
        filename:join([SystemConfigDir, "config", ?ZOTONIC_VERSION]),
        filename:join([SystemConfigDir, "config", MinorVersion]),
        filename:join([SystemConfigDir, "config", MajorVersion]),
        filename:join([SystemConfigDir, "config" ])
    ],
    Locs = HomeLocs ++ EtcLocs ++ SystemLocs,
    case lists:dropwhile(fun is_empty_config_dir/1, Locs) of
        [] ->
            % Use the OS specific default
            ZotonicDir = filename:join([SystemConfigDir, "config"]),
            VersionDir = filename:join([SystemConfigDir, "config", MajorVersion]),
            % The '$HOME/.config' dir is not pre-created on some Linux VMs
            _ = z_filelib:ensure_dir(SystemConfigDir),
            case file:make_dir(SystemConfigDir) of
                ok -> file:change_mode(SystemConfigDir, 8#00700);
                {error, _} -> ok
            end,
            case file:make_dir(ZotonicDir) of
                ok -> file:change_mode(ZotonicDir, 8#00700);
                {error, _} -> ok
            end,
            case file:make_dir(VersionDir) of
                ok -> file:change_mode(VersionDir, 8#00700);
                {error, _} -> ok
            end,
            case filelib:is_dir(VersionDir) of
                true ->
                    {ok, VersionDir};
                false ->
                    ?LOG_ERROR(#{
                        text => <<"Could not create config directory">>,
                        in => zotonic_core,
                        path => VersionDir,
                        result => error,
                        reason => not_directory
                    }),
                    {error, enoent}
            end;
        [ D | _ ] ->
            {ok, D}
    end.

%% @doc If there is a file starting with 'zotonic' in the directory, then we assume
%% the directory is in use for configuration files. Otherwise check another directory
%% for the presence of zotonic config files.
is_empty_config_dir(Dir) ->
    files(Dir, "zotonic*") =:= [].

base_nodename(Node) ->
    lists:takewhile(fun(C) -> C =/= $@ end, atom_to_list(Node)).

%% @doc List all (regular) files in a directory, skip hidden and temp files.
-spec files( file:filename_all() ) -> [ file:filename_all() ].
files(Dir) ->
    files(Dir, "*").

%% @doc List all (regular) files in a directory, skip hidden and temp files.
%%      Ensures the list of files is sorted in a consistent way.
-spec files( file:filename_all(), string() ) -> [ file:filename_all() ].
files(Dir, Wildcard) ->
    Files = filelib:wildcard( filename:join(Dir, Wildcard) ),
    lists:sort(
        lists:filter(
            fun(F) ->
                case filename:basename(F) of
                    "." ++ _ -> false;
                    _ ->
                        not filelib:is_dir(F)
                        andalso lists:last(F) =/= $#
                        andalso lists:last(F) =/= $~
                end
            end,
            Files)).

%% @doc Read a config file, return a list of the contents.
%%      The file can be in erlang, yaml, or json format.
-spec consult( file:filename_all() ) -> {ok, list( map() | proplists:proplist() )} | {error, term()}.
consult(File) ->
    case z_convert:to_list( filename:extension(File) ) of
        ".config" ->
            consult_erlang(File);
        ".erlang" ->
            consult_erlang(File);
        ".yml" ->
            consult_yaml(File);
        ".yaml" ->
            consult_yaml(File);
        ".json" ->
            consult_json(File);
        _Other ->
            {error, {config_file, unknown_format, File, undefined}}
    end.

consult_erlang(File) ->
    case file:consult(File) of
        {ok, L} when is_list(L) ->
            {ok, L};
        {error, Reason} when is_atom(Reason) ->
            {error, {config_file, Reason, File, undefined}};
        {error, Reason} ->
            {error, {config_file, consult_error, File, Reason}}
    end.

consult_yaml(File) ->
    try
        Options = [
            str_node_as_binary,
            {map_node_format, map}
        ],
        Data = yamerl_constr:file(File, Options),
        {ok, atomify_labels(Data)}
    catch
        throw:#yamerl_exception{} = E ->
            {error, {config_file, yaml_format, File, E}}
    end.

consult_json(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            try
                case jsx:decode(Data, [ return_maps, {labels, atom} ]) of
                    Map when is_map(Map) ->
                        {ok, [ Map ]};
                    [ Map | _ ] = List when is_map(Map) ->
                        {ok, List};
                    _ ->
                        {error, {config_file, no_list_or_map, File, undefined}}
                end
            catch
                _:Reason ->
                    {error, {config_file, json_format, File, Reason}}
            end;
        {error, Reason} ->
            {error, {config_file, Reason, File, undefined}}
    end.

split_version(Version) ->
    {match, [Minor, Major]} = re:run(Version, "(([0-9]+)\\.[0-9]+)", [{capture, all_but_first, list}]),
    {Major, Minor}.

atomify_labels(Data) when is_list(Data) ->
    lists:map(fun atomify_labels/1, Data);
atomify_labels(Data) when is_map(Data) ->
    L = lists:map(
        fun({K, V}) ->
            {binary_to_atom(K, utf8), atomify_labels(V)}
        end,
        maps:to_list(Data)),
    maps:from_list(L);
atomify_labels(V) ->
    V.
