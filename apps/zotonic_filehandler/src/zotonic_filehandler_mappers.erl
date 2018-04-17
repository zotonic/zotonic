%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014-2017 Arjan Scherpenisse
%%
%% @doc Handle changed files

%% Copyright 2014-2017 Arjan Scherpenisse
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

-module(zotonic_filehandler_mappers).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-export([
    mappers/0
]).

%% Actions, do not use from other modules.
-export([
    compile_yecc/1,
    compile_sass/2,
    compile_coffee/2,
    compile_less/2,
    run_build/2
]).


-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").


-spec mappers() -> [ function() ].
mappers() ->
    Builtin = [
        fun drop_dirs/7,
        fun temp_beam/7,
        fun beam_file/7,
        fun app_file/7,
        fun header_file/7,
        fun erlang_file/7,
        fun yecc/7,
        fun lib_src_build/7,
        fun sass_file/7,
        fun coffee_file/7,
        fun less_file/7
    ],
    zotonic_notifier:foldl(
        ?SYSTEM_NOTIFIER, zotonic_filehandler_mappers,
        zotonic_filehandler_mappers, Builtin,
        undefined).


%% ------------------------------ Map files to actions ------------------------------

drop_dirs(delete, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false;
drop_dirs(_Verb, _Application, _What, _Ext, _Root, _Split, Filename) ->
    case filelib:is_dir(Filename) of
        true -> ok;
        false -> false
    end.


%% @doc Recompile Erlang files on the fly
temp_beam(_Verb, _Application, _What, <<".bea#">>, _Root, _Split, _Filename) ->
    ok;
temp_beam(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


beam_file(delete, _Application, _What, <<".beam">>, _Root, _Split, _Filename) ->
    ok;
beam_file(_Verb, _Application, {ebin, _EbinFile}, <<".beam">>, Root, _Split, _Filename) ->
    {ok, [
        {zotonic_filehandler_compile, ld, [erlang:binary_to_atom(Root, utf8)]}
    ]};
beam_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


%% @doc Check for newly created/added Erlang applications
app_file(create, _Application, {app, _AppFile}, <<".app">>, _Root, _Split, Filename) ->
    {ok, [
        {zotonic_filehandler_compile, code_path_check, [Filename]}
    ]};
app_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


header_file(delete, _Basename, _What, <<".hrl">>, _Root, _Split, _Filename) ->
    ok;
header_file(_Verb, _Basename, _What, <<".hrl">>, _Root, _Split, _Filename) ->
    {ok, [
        {zotonic_filehandler_compile, all, []}
    ]};
header_file(_Verb, _Basename, _What, _Ext, _Root, _Split, _Filename) ->
    false.


erlang_file(delete, _Application, _What, <<".erl">>, _Root, _Split, _Filename) ->
    % Should delete the beam file
    ok;
erlang_file(_Verb, _Application, {src, _Path}, <<".erl">>, _Root, _Split, Filename) ->
    {ok, [
        {zotonic_filehandler_compile, recompile, [Filename]}
    ]};
erlang_file(_Verb, _Application, {test, _Path}, <<".erl">>, _Root, _Split, Filename) ->
    {ok, [
        {zotonic_filehandler_compile, recompile, [Filename]}
    ]};
erlang_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


yecc(delete, _Application, _What, <<".yrl">>, _Root, _Split, _Filename) ->
    % Should delete the erlang file
    ok;
yecc(_Verb, _Application, {src, _Path}, <<".yrl">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, compile_yecc, [Filename]}
    ]};
yecc(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


lib_src_build(_Verb, Application, {priv, <<"lib-src">>, Path}, _Ext, _Root, _Split, _Filename) ->
    case build_command(Application, Path) of
        {ok, BuildCmd} ->
            {ok, [
                {?MODULE, run_build, [Application, BuildCmd]}
            ]};
        ok ->
            ok;
        false ->
            false
    end;
lib_src_build(_Verb, _Application, _Path, _Ext, _Root, _Split, _Filename) ->
    false.

sass_file(delete, _Application, _What, Ext, _Root, _Split, _Filename)
    when Ext =:= <<".sass">>; Ext =:= <<".scss">> ->
    % Should delete the css file
    ok;
sass_file(_Verb, Application, {priv, <<"lib-src">>, Path}, Ext, _Root, _Split, _Filename)
    when Ext =:= <<".sass">>; Ext =:= <<".scss">> ->
    {ok, [
        {?MODULE, compile_sass, [Application, Path]}
    ]};
sass_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


less_file(delete, _Application, _What, <<".less">>, _Root, _Split, _Filename) ->
    % Should delete the css file
    ok;
less_file(_Verb, Application, {priv, <<"lib-src">>, Path}, <<".less">>, _Root, _Split, _Filename) ->
    {ok, [
        {?MODULE, compile_less, [Application, Path]}
    ]};
less_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


coffee_file(delete, _Application, _What, <<".coffee">>, _Root, _Split, _Filename) ->
    % Should delete the js file
    ok;
coffee_file(_Verb, Application, {priv, <<"lib-src">>, Path}, <<".coffee">>, _Root, _Split, _Filename) ->
    {ok, [
        {?MODULE, compile_coffee, [Application, Path]}
    ]};
coffee_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


%% ------------------------------ Action callbacks -----------------------------------

compile_yecc(Filename) ->
    % "Rebuilding yecc file: " ++ filename:basename(Filename);
    InPath = unicode:characters_to_list(Filename, utf8),
    OutPath = filename:rootname(InPath) ++ ".erl",
    case is_newer(InPath, OutPath) of
        true ->
            zotonic_filehandler:terminal_notifier("Yecc: " ++ filename:basename(InPath)),
            Cmd = "erlc -o "
                ++ z_utils:os_escape(filename:dirname(InPath))
                ++ " "
                ++ z_utils:os_escape(InPath),
            zotonic_filehandler_compile:run_cmd(Cmd);
        false ->
            ok
    end.


%% @doc SCSS / SASS files from priv/lib-src/css/.../foo.sass -> priv/lib/css/.../foo.css
compile_sass(Application, SrcPath) ->
    AppPriv = code:priv_dir(Application),
    InPath = filename:join([AppPriv, "lib-src", SrcPath]),
    DstPath = unicode:characters_to_list(filename:rootname(SrcPath)) ++ ".css",
    OutPath = filename:join([ AppPriv, "lib", DstPath ]),
    case is_newer(InPath, OutPath) of
        true ->
            case z_filelib:ensure_dir(OutPath) of
                ok ->
                    zotonic_filehandler:terminal_notifier("Sass: " ++ filename:basename(InPath)),
                    Cmd = [
                        "sass -C --sourcemap=none --update ",
                        z_utils:os_escape(InPath),
                        ":",
                        z_utils:os_escape(OutPath)
                    ],
                    zotonic_filehandler_compile:run_cmd(Cmd);
                {error, _} = Error ->
                    lager:error("Could not create directory for ~p", [OutPath]),
                    Error
            end;
        false ->
            ok
    end.


%% @doc LESS files from priv/lib-src/css/.../foo.less -> priv/lib/css/.../foo.css
%%      Check for a 'config' file on the path, if present then that file is used
%%      for the compilation of the less files.
compile_less(Application, SrcPath) ->
    AppPriv = code:priv_dir(Application),
    InPath = filename:join([AppPriv, "lib-src", SrcPath]),
    DstPath = unicode:characters_to_list(filename:rootname(SrcPath)) ++ ".css",
    OutPath = filename:join([ AppPriv, "lib", DstPath ]),
    case is_newer(InPath, OutPath) of
        true ->
            zotonic_filehandler:terminal_notifier("Lessc: " ++ filename:basename(InPath)),
            case z_filelib:ensure_dir(OutPath) of
                ok ->
                    Cmd = [
                        "lessc ",
                        z_utils:os_escape(InPath),
                        " > ",
                        z_utils:os_escape(OutPath)
                    ],
                    zotonic_filehandler_compile:run_cmd(Cmd);
                {error, _} = Error ->
                    lager:error("Could not create directory for ~p", [OutPath]),
                    Error
            end;
        false ->
            ok
    end.


%% @doc coffee files from priv/lib-src/js/.../foo.coffee -> priv/lib/js/.../foo.js
compile_coffee(Application, SrcPath) ->
    AppPriv = code:priv_dir(Application),
    InPath = filename:join([AppPriv, "lib-src", SrcPath]),
    DstPath = unicode:characters_to_list(filename:rootname(SrcPath)) ++ ".js",
    OutPath = filename:join([ AppPriv, "lib", DstPath ]),
    case is_newer(InPath, OutPath) of
        true ->
            case z_filelib:ensure_dir(OutPath) of
                ok ->
                    zotonic_filehandler:terminal_notifier("Coffee: " ++ filename:basename(InPath)),
                    Cmd = [
                        "coffee -o ",
                        z_utils:os_escape(OutPath),
                        " -c ",
                        z_utils:os_escape(InPath)
                    ],
                    zotonic_filehandler_compile:run_cmd(Cmd);
                {error, _} = Error ->
                    lager:error("Could not create directory for ~p", [OutPath]),
                    Error
            end;
        false ->
            ok
    end.

%% @doc Run the build command in a lib-src directory
run_build(Application, {make, Makefile}) ->
    zotonic_filehandler:terminal_notifier("Make: " ++ app_path(Application, Makefile)),
    CmdOpts = [
        {env, [
            {"APP_DIR", code:lib_dir(Application)},
            {"ZOTONIC_LIB", "1"}
        ]}
    ],
    BuildDir = filename:dirname(Makefile),
    MakeCmd = "cd " ++ z_utils:os_escape(BuildDir) ++ "; sh -c make " ++ z_utils:os_escape(Makefile),
    zotonic_filehandler_compile:run_cmd(MakeCmd, CmdOpts).

%% ---------------------------------------- Support routines ------------------------------

app_path(Application, BuildCmd) ->
    AppB = atom_to_binary(Application, utf8),
    case binary:split(BuildCmd, <<"/", AppB/binary, "/">>) of
        [_, X] ->
            unicode:characters_to_list(iolist_to_binary([ AppB, "/", X]));
        [_] ->
            unicode:characters_to_list(BuildCmd)
    end.

is_newer(In, Out) ->
    InMod = filelib:last_modified(In),
    OutMod = filelib:last_modified(Out),
    is_newer_1(InMod, OutMod).

is_newer_1(0, _) -> false;
is_newer_1(_, 0) -> true;
is_newer_1(A, B) -> A > B.


%% @doc Find a Makefile on the path to the changed lib-src file.
%%      We look for a Makefile to build the targets in 'priv/lib'. This Makefile will
%%      be executed in the next step (after deduplication).
%%      TODO: Support buildwatchers, maybe with "make watcher"
build_command(Application, SrcPath) ->
    LibSrcDir = filename:join([code:priv_dir(Application), "lib-src"]),
    case find_build(LibSrcDir, filename:split(filename:dirname(SrcPath))) of
        {ok, {make, _Makefile} = BuildCmd} ->
            {ok, BuildCmd};
        false ->
            false
    end.

find_build(_LibSrcDir, []) ->
    false;
find_build(LibSrcDir, Dir) ->
    case lists:last(Dir) of
        <<"dist">> -> false;
        _ ->
            Dirname = filename:join([LibSrcDir] ++ Dir),
            Makefile = filename:join(Dirname, "Makefile"),
            case filelib:is_file(Makefile) of
                true ->
                    {ok, {make, Makefile}};
                false ->
                    Up = lists:reverse(tl(lists:reverse(Dir))),
                    find_build(LibSrcDir, Up)
            end
    end.
