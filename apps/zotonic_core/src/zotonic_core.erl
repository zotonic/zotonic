%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2017 Marc Worrell
%% @doc Zotonic core - main routines to 'reason' about the current Zotonic
%%      installation.

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

-module(zotonic_core).

-author('Marc Worrell <marc@worrell.nl>').

-export([
    is_zotonic_project/0,
    is_testsandbox/0,
    is_app_available/1,

    setup/0
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

%% @doc Initial setup before starting Zotonic
-spec setup() -> ok.
setup() ->
    io:setopts([{encoding, unicode}]),
    z_jsxrecord:init(),
    ensure_mnesia_schema().


%% @doc Ensure that mnesia has created its schema in the configured priv/data/mnesia directory.
-spec ensure_mnesia_schema() -> ok.
ensure_mnesia_schema() ->
    case mnesia_dir() of
        {ok, Dir} ->
            case filelib:is_dir(Dir) andalso filelib:is_regular(filename:join(Dir, "schema.DAT")) of
                true -> ok;
                false -> ok = mnesia:create_schema([node()])
            end;
        undefined ->
            lager:info("No mnesia directory defined, running without persistent email queue and filezcache. "
                       "To enable persistency, add to erlang.config: {mnesia,[{dir,\"priv/mnesia\"}]}"),
            ok
    end.

mnesia_dir() ->
    application:load(mnesia),
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
        {ok, "priv/mnesia"} -> mnesia_priv_dir();
        {ok, Dir} -> {ok, Dir};
        undefined -> mnesia_priv_dir()
    end.

mnesia_priv_dir() ->
    PrivDir = case code:priv_dir(zotonic) of
        {error, bad_name} -> code:priv_dir(zotonic_core);
        ZotonicPrivDir when is_list(ZotonicPrivDir) -> ZotonicPrivDir
    end,
    mnesia_dir_append_node(filename:join([ PrivDir, "mnesia" ])).

mnesia_dir_append_node(Dir) ->
    MnesiaDir = filename:join([ Dir, atom_to_list(node()) ]),
    case z_filelib:ensure_dir(MnesiaDir) of
        ok ->
            application:set_env(mnesia, dir, MnesiaDir),
            {ok, MnesiaDir};
        {error, _} = Error ->
            lager:error("Could not create mnesia dir \"~s\": ~p",
                        [MnesiaDir, Error]),
            undefined
    end.
