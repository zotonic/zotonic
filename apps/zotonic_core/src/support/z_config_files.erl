%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2019 Marc Worrell
%% @doc Generic support for finding and parsing config files.

%% Copyright 2019 Marc Worrell
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


%% @doc Find the configuration directory for certificates
-spec security_dir() -> {ok, file:filename_all()} | {error, term()}.
security_dir() ->
    case z_config:get(security_dir) of
        undefined ->
            case find_security_dir(system, filename:join([ "etc", "zotonic", "security" ])) of
                {ok, Dir} ->
                    {ok, Dir};
                {error, _} ->
                    Home = os:getenv("HOME"),
                    case find_security_dir(home, filename:join([ Home, ".zotonic", "security" ])) of
                        {ok, Dir} ->
                            {ok, Dir};
                        {error, _} ->
                            find_security_dir(home, filename:join([ z_utils:lib_dir(priv), "security" ]))
                    end
            end;
        Dir ->
            {ok, Dir}
    end.

find_security_dir(Type, Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            {ok, Dir};
        false when Type =:= home ->
            case z_filelib:ensure_dir(filename:join([Dir, ".empty"])) of
                ok ->
                    _ = file:change_mode(Dir, 8#00700),
                    {ok, Dir};
                {error, _} = Error ->
                    Error
            end;
        false when Type =:= system ->
            case filelib:is_dir( filename:basename(Dir) ) of
                true ->
                    case z_filelib:ensure_dir(filename:join([Dir, ".empty"])) of
                        ok ->
                            _ = file:change_mode(Dir, 8#00700),
                            {ok, Dir};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, enoent}
            end
    end.


%% @doc Find the configuration directory for current running Zotonic.
-spec config_dir() -> {ok, file:filename_all()} | {error, term()}.
config_dir() ->
    config_dir( node() ).

%% @doc Find the configuration directory for the given Zotonic node.
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
    EtcLocs = [
        filename:join(["/etc/zotonic", atom_to_list(Node)]),
        filename:join(["/etc/zotonic", base_nodename(Node)]),
        filename:join(["/etc/zotonic", ?ZOTONIC_VERSION]),
        filename:join(["/etc/zotonic", MinorVersion]),
        filename:join(["/etc/zotonic", MajorVersion]),
        filename:join(["/etc/zotonic"])
    ],
    Locations = HomeLocs ++ EtcLocs,
    Found = lists:dropwhile(
        fun(D) -> z_config_files:files(D, "zotonic*") =:= [] end,
        Locations),
    case Found of
        [] when is_list(Home), Home =/= "" ->
            Dir = filename:join([Home, ".zotonic", MajorVersion]),
            ZotonicDir = filename:join([Home, ".zotonic"]),
            case file:make_dir(ZotonicDir) of
                ok -> file:change_mode(ZotonicDir, 8#00700);
                {error, _} -> ok
            end,
            case file:make_dir(Dir) of
                ok -> file:change_mode(Dir, 8#00700);
                {error, _} -> ok
            end,
            case filelib:is_dir(Dir) of
                true ->
                    {ok, Dir};
                false ->
                    {error, enoent}
            end;
        [] ->
            {error, enoent};
        [ Dir | _ ] ->
            {ok, Dir}
    end.

base_nodename(Node) ->
    lists:takewhile(fun(C) -> C =/= $@ end, atom_to_list(Node)).

%% @doc List all (regular) files in a directory, skip hidden en temp files.
-spec files( file:filename_all() ) -> [ file:filename_all() ].
files(Dir) ->
    files(Dir, "*").

%% @doc List all (regular) files in a directory, skip hidden en temp files.
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
    case filename:extension(File) of
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
