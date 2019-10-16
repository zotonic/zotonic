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

-spec config_dir() -> {ok, file:filename_all()} | {error, enoent}.
config_dir() ->
    config_dir(node()).

config_dir(Node) when is_atom(Node) ->
    maybe_initialize( z_config_files:config_dir(Node) ).

maybe_initialize({ok, "/etc/" ++ _} = Ok) ->
    Ok;
maybe_initialize({ok, Dir}) ->
    maybe_initialize_configs(Dir),
    {ok, Dir};
maybe_initialize({error, _} = Error) ->
    Error.

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
            Files = lists:filter(fun(F) -> not is_erlang_config(F) end, z_config_files:files(Dir)),
            SubFiles= z_config_files:files( filename:join([ Dir, "config.d" ]) ),
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
            Files = z_config_files:files(Dir),
            lists:filter(fun(F) -> is_erlang_config(F) end, Files);
        {error, _} ->
            []
    end.

config_files(Node) ->
    case config_dir(Node) of
        {ok, Dir} ->
            Files = z_config_files:files(Dir),
            SubFiles= z_config_files:files( filename:join([ Dir, "config.d" ]) ),
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
                case z_config_files:consult(F) of
                    {ok, Data} ->
                        apps_config(F, Data, Acc);
                    {error, _} = Error ->
                        Error
                end
        end,
        {ok, #{}},
        Fs).

apps_config(File, Data, Cfgs) when is_list(Data) ->
    lists:foldl(
        fun
            (AppConfig, Acc) when is_map(AppConfig) ->
                maps:fold(
                    fun
                        (App, Cfg, {ok, MAcc}) ->
                            app_config(File, App, Cfg, MAcc);
                        (_App, _Cfg, {error, _} = Error) ->
                            Error
                    end,
                    {ok, Acc},
                    AppConfig);
            (AppConfig, Acc) when is_list(AppConfig) ->
                lists:foldl(
                    fun
                        ({App, Cfg}, {ok, MAcc}) ->
                            app_config(File, App, Cfg, MAcc);
                        (Other, {ok, _}) ->
                            {error, {config_file, format, File, {unknown_term, Other}}};
                        (_, {error, _} = Error) ->
                            Error
                    end,
                    {ok, Acc},
                    AppConfig)
        end,
        Cfgs,
        Data);
apps_config(File, _Data, _Cfgs) ->
    error_logger:error_msg("Config file '~s' does not contain a list of configurations.",
                           [File]),
    {error, {config_file, missing_list_map, File, undefined}}.

app_config(_File, App, Data, Acc) when is_map(Data), is_atom(App) ->
    _ = application:load(App),
    {ok, Acc#{ App => maps:get(App, Acc, #{}) }};
app_config(File, App, Data, Acc) when is_list(Data), is_atom(App) ->
    Map = flatten_to_map(Data, #{}),
    app_config(File, App, Map, Acc).

% Flatten a nested list of proplists to a map.
flatten_to_map(Data, Map) ->
    lists:foldl(
        fun
            ({K, V}, Acc) ->
                Acc#{ K => V };
            (K, Acc) ->
                Acc#{ K => true }
        end,
        Map,
        lists:flatten(Data)).
