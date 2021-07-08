%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019-2021 Marc Worrell
%% @doc Generic support for finding and parsing config files.

%% Copyright 2019-2021 Marc Worrell
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
    security_dir/0,
    config_dir/0,
    config_dir/1,
    files/1,
    files/2,
    consult/1
]).

-include_lib("zotonic_core/include/zotonic_release.hrl").
-include_lib("yamerl/include/yamerl_errors.hrl").


%% @doc Find the default directory for certificates and other secrets.
%% Checks the following locations:
%%
%% <ol>
%%   <li>The configuration <tt>security_dir</tt></li>
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
    case z_config:get(security_dir) of
        undefined ->
            security_dir_1();
        Dir ->
            {ok, Dir}
    end.

security_dir_1() ->
    Home = os:getenv("HOME"),
    HomeLocs = case Home of
        false -> [];
        "" -> [];
        _ ->
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
                    {ok, SecurityDir};
                false ->
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
%%   <li>The envirpnment variable <tt>ZOTONIC_CONFIG_DIR</tt></li>
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
    Home = os:getenv("HOME"),
    {MajorVersion, MinorVersion} = split_version(?ZOTONIC_VERSION),
    HomeLocs = case Home of
        false -> [];
        "" -> [];
        _ ->
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
