%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017-2019 Marc Worrell
%% @doc Locate and read config files, especially the "zotonic.config"

%% Copyright 2017-2019 Marc Worrell
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

-module(zotonic_launcher_config).

-export([
    config_dir/0,
    config_dir/1,

    config_files/1,
    load_configs/1,
    read_configs/1,

    zotonic_config_files/1,
    erlang_config_files/1
]).

-include_lib("zotonic_core/include/zotonic_release.hrl").
-include_lib("yamerl/include/yamerl_errors.hrl").


-spec config_dir() -> {ok, file:filename_all()} | {error, enoent}.
config_dir() ->
    config_dir(node()).

config_dir(Node) when is_atom(Node) ->
    case os:getenv("ZOTONIC_CONFIG_DIR") of
        false -> config_dir_init(Node);
        "" -> config_dir_init(Node);
        Dir ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

config_dir_init(Node) ->
    case proplists:get_value(zotonic_config_dir, init:get_arguments()) of
        undefined ->
            config_dir_fs(Node);
        [] ->
            config_dir_fs(Node);
        [ Dir ] ->
            case filelib:is_dir(Dir) of
                true -> {ok, Dir};
                false -> {error, enoent}
            end
    end.

config_dir_fs(Node) ->
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
        fun(D) -> files(D, "zotonic*") =:= [] end,
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
                    maybe_initialize_configs(Dir),
                    {ok, Dir};
                false ->
                    {error, enoent}
            end;
        [] ->
            {error, enoent};
        [ "/etc/" ++ _ = Dir | _ ] ->
            {ok, Dir};
        [ Dir | _ ] ->
            maybe_initialize_configs(Dir),
            {ok, Dir}
    end.

base_nodename(Node) ->
    lists:takewhile(fun(C) -> C =/= $@ end, atom_to_list(Node)).

-spec maybe_initialize_configs( file:filename_all() ) -> ok.
maybe_initialize_configs(Dir) ->
    SourceDir = case code:priv_dir(zotonic_launcher) of
        {error, bad_name} ->
            Beam = code:where_is_file("zotonic_launcher_config.beam"),
            Top = filename:dirname( filename:dirname(Beam) ),
            filename:join([ Top, "priv", "config" ]);
        PrivDir ->
            filename:join([ PrivDir, "config" ])
    end,
    case filelib:is_file(filename:join([ Dir, "erlang.config" ])) of
        false ->
            copyfile("erlang.config", SourceDir, Dir);
        true ->
            ok
    end,
    case filelib:is_file(filename:join([ Dir, "zotonic.config" ])) of
        false ->
            copyfile("zotonic.config", SourceDir, Dir);
        true ->
            ok
    end,
    ok.

copyfile(Filename, FromDir, ToDir) ->
    File = filename:join([ FromDir, Filename ++ ".in" ]),
    case filelib:is_file(File) of
        true ->
            case file:read_file(File) of
                {ok, Data} ->
                    Data1 = replace_placeholders(Data),
                    TargetFile = filename:join([ ToDir, Filename ]),
                    file:write_file(TargetFile, Data1),
                    file:change_mode(TargetFile, 8#00600);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, enoent}
    end.

replace_placeholders(Data) ->
    binary:replace(Data, <<"%%GENERATED%%">>, z_ids:id(16), [ global ]).


%% Zotonic config files are "zotonic.*" files in the root of the
%% config directory and all files the 'config.d' subdirectory.
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

%% Erlang config files must be in the root of the config directory.
%% They must also have the extension ".config" and not be called "zotonic.config"
-spec erlang_config_files( node() ) -> list( file:filename() ).
erlang_config_files(Node) ->
    case config_dir(Node) of
        {ok, Dir} ->
            Files = files(Dir),
            lists:filter(fun(F) -> is_erlang_config(F) end, Files);
        {error, _} ->
            []
    end.

config_files(Node) ->
    case config_dir(Node) of
        {ok, Dir} ->
            Files = files(Dir),
            SubFiles= files( filename:join([ Dir, "config.d" ]) ),
            Files ++ SubFiles;
        {error, _} ->
            []
    end.

files(Dir) ->
    files(Dir, "*").

files(Dir, Wildcard) ->
    Files = filelib:wildcard( filename:join(Dir, Wildcard) ),
    lists:filter(
        fun(F) ->
            case filename:basename(F) of
                "." ++ _ -> false;
                _ -> not filelib:is_dir(F)
            end
        end,
        Files).

is_erlang_config(F) ->
    case filename:basename(F) of
        "zotonic" -> false;
        "zotonic." ++ _ -> false;
        _ -> filename:extension(F) == ".config"
    end.

-spec load_configs( map() ) -> ok.
load_configs( Cfgs ) when is_map(Cfgs) ->
    maps:fold(
        fun(App, KVs, _) ->
            _ = application:load(App),
            maps:fold(
                fun(K, V, _) ->
                    application:set_env(App, K, V)
                end,
                ok,
                KVs)
        end,
        ok,
        Cfgs).

-spec read_configs( [ file:filename_all() ] ) -> {ok, #{ atom() => map() }} | {error, term()}.
read_configs(Fs) when is_list(Fs) ->
    lists:foldl(
        fun
            (_, {error, _} = Error) ->
                Error;
            (F, {ok, Acc}) ->
                case consult_config(F) of
                    {ok, Data} ->
                        app_config(F, Data, Acc);
                    {error, _} = Error ->
                        Error
                end
        end,
        {ok, #{}},
        Fs).

app_config(File, Data, Cfgs) when is_map(Data) ->
    maps:fold(
        fun
            (_, _, {error, _} = Error) ->
                Error;
            (K, Vs, {ok, Acc}) ->
                App = z_convert:to_atom(K),
                app_config(File, App, Vs, Acc)
        end,
        {ok, Cfgs},
        Data);
app_config(File, Data, Cfgs) when is_list(Data) ->
    Map = to_map(Data),
    app_config(File, Map, Cfgs);
app_config(File, _Data, _Cfgs) ->
    error_logger:error_msg("Config file '~s' does not contain a list or a map.",
                           [File]),
    {error, {config_file, missing_list_map, File, undefined}}.

app_config(_File, App, Data, Acc) when is_map(Data), is_atom(App) ->
    application:load(App),
    AppCfg = maps:get(App, Acc, #{}),
    AppCfgNew = maps:fold(
        fun
            (K, Vs, AppAcc) ->
                K1 = z_convert:to_atom(K),
                AppAcc#{ K1 => Vs }
        end,
        AppCfg,
        Data),
    {ok, Acc#{ App => AppCfgNew }};
app_config(File, App, Data, Acc) when is_list(Data), is_atom(App) ->
    Map = to_map(Data),
    app_config(File, App, Map, Acc).

consult_config(File) ->
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
        {ok, L} when is_list(L); is_map(L) ->
            {ok, L};
        {error, Reason} when is_atom(Reason) ->
            {error, {config_file, Reason, File, undefined}};
        {error, Reason} ->
            {error, {config_file, consult_error, File, Reason}}
    end.

consult_yaml(File) ->
    try
        yamerl_constr:file(File, [ str_node_as_binary, {map_node_format, map} ])
    catch
        throw:#yamerl_exception{} = E ->
            {error, {config_file, yaml_format, File, E}}
    end.

consult_json(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            try
                jsxrecord:decode(Data)
            catch
                _:Reason ->
                    {error, {config_file, json_format, File, Reason}}
            end;
        {error, Reason} ->
            {error, {config_file, Reason, File, undefined}}
    end.

to_map(Data) when is_list(Data) ->
    to_map(Data, #{}).

to_map(Data, Map) ->
    lists:foldl(
        fun
            (L, Acc) when is_list(L) ->
                to_map(L, Acc);
            ({K, V}, Acc) ->
                K1 = z_convert:to_atom(K),
                Acc#{ K1 => V };
            (K, Acc) ->
                K1 = z_convert:to_atom(K),
                Acc#{ K1 => true }
        end,
        Map,
        Data).

split_version(Version) ->
    {match, [Minor, Major]} = re:run(Version, "(([0-9]+)\\.[0-9]+)", [{capture, all_but_first, list}]),
    {Major, Minor}.
