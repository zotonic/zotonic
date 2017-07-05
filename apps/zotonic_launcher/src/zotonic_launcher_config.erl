%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Locate and read config files, especially the "zotonic.config"

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

-module(zotonic_launcher_config).

-export([
    load_configs/0,
    config_files/0,
    zotonic_config_file/0,
    erlang_config_file/0,
    locate_config_file/1
]).

-include_lib("zotonic_core/include/zotonic_release.hrl").


-spec load_configs() -> ok.
load_configs() ->
    lists:foreach(
        fun load_config_file/1,
        config_files()),
    ok.

config_files() ->
    Files = [ F || [F] <- proplists:get_all_values(config, init:get_arguments()) ],
    Files1 = add_erlang_config_file(Files),
    add_zotonic_config_file(Files1).

add_zotonic_config_file(Files) ->
    case find_zotonic_config(Files) of
        false ->
            case locate_config_file("zotonic.config") of
                {ok, CfgFile} -> Files ++ [CfgFile];
                false -> Files
            end;
        {ok, _} ->
            Files
    end.

add_erlang_config_file(Files) ->
    case find_erlang_config(Files) of
        false ->
            case locate_config_file("erlang.config") of
                {ok, CfgFile} -> Files ++ [CfgFile];
                false -> Files
            end;
        {ok, _} ->
            Files
    end.

load_config_file(File) ->
    case file:consult(File) of
        {ok, Terms} when is_list(Terms) ->
            lists:foreach(
                fun(T) ->
                    handle_config(File, T)
                end,
                Terms);
        {error, _} = Error ->
            lager:error("Error reading configuration file \"~s\": ~p",
                        [File, Error]),
            Error
    end.


handle_config(File, L) when is_list(L) ->
    lists:foreach(
        fun(T) ->
            handle_config(File, T)
        end,
        L);
handle_config(File, {App, L}) when is_atom(App), is_list(L) ->
    lists:foreach(
        fun
            ({Conf, Val}) when is_atom(Conf) ->
                application:set_env(App, Conf, Val);
            (Other) ->
                lager:error("Unexpected config term in \"~s\" for ~p: ~p",
                            [File, App, Other])
        end,
        L).

% Places to look for the config file:
% - ``$HOME/.zotonic/(nodename)/``
% - ``$HOME/.zotonic/(version)/``
% - ``$HOME/.zotonic/``
% - ``/etc/zotonic/(nodename)/``
% - ``/etc/zotonic/(version)/``
% - ``/etc/zotonic/``

%% @doc Return the zotonic config file to be used
-spec zotonic_config_file() -> {ok, file:filename()} | false.
zotonic_config_file() ->
    Files = proplists:get_all_values(config, init:get_arguments()),
    case find_zotonic_config(Files) of
        false -> locate_config_file("zotonic.config");
        {ok, File} -> File
    end.

find_zotonic_config([]) -> false;
find_zotonic_config([F|Fs]) ->
    case filename:basename(F) of
        "zotonic" -> {ok, F ++ ".config"};
        "zotonic." ++ _ -> {ok, F};
        _ -> find_zotonic_config(Fs)
    end.

%% @doc Return the erlang config file to be used
-spec erlang_config_file() -> {ok, file:filename()} | false.
erlang_config_file() ->
    Files = proplists:get_all_values(config, init:get_arguments()),
    case find_erlang_config(Files) of
        false -> locate_config_file("erlang.config");
        {ok, File} -> File
    end.

find_erlang_config([]) -> false;
find_erlang_config([F|Fs]) ->
    case filename:basename(F) of
        "erlang" -> {ok, F ++ ".config"};
        "erlang." ++ _ -> {ok, F};
        _ -> find_erlang_config(Fs)
    end.

-spec locate_config_file(string()) -> {ok, file:filename()} | false.
locate_config_file(ConfigFile) ->
    Home = os:getenv("HOME"),
    {MajorVersion, MinorVersion} = split_version(?ZOTONIC_VERSION),
    Locations = [
        filename:join([Home, ".zotonic", atom_to_list(node()), ConfigFile]),
        filename:join([Home, ".zotonic", ?ZOTONIC_VERSION, ConfigFile]),
        filename:join([Home, ".zotonic", MinorVersion, ConfigFile]),
        filename:join([Home, ".zotonic", MajorVersion, ConfigFile]),
        filename:join([Home, ".zotonic", ConfigFile]),

        filename:join(["/etc/zotonic", atom_to_list(node()), ConfigFile]),
        filename:join(["/etc/zotonic", ?ZOTONIC_VERSION, ConfigFile]),
        filename:join(["/etc/zotonic", MinorVersion, ConfigFile]),
        filename:join(["/etc/zotonic", MajorVersion, ConfigFile]),
        filename:join(["/etc/zotonic", ConfigFile])
    ],
    case lists:dropwhile(
        fun(Path) ->
            not filelib:is_regular(Path)
        end,
        Locations)
    of
        [] -> false;
        [P|_] -> {ok, P}
    end.

split_version(Version) ->
    {match, [Minor, Major]} = re:run(Version, "(([0-9]+)\\.[0-9]+)", [{capture, all_but_first, list}]),
    {Major, Minor}.
