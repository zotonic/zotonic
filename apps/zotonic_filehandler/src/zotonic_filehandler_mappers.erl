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
    reindex_modules/0,
    reload_translations/0,
    reload_dispatch/0,
    mark_modified/1,
    run_build/2
]).


-spec mappers() -> [ function() ].
mappers() ->
    [
        fun temp_beam/7,
        fun beam_file/7,
        fun header_file/7,
        fun erlang_file/7,
        fun yecc/7,
        fun lib_src_build/7,
        fun sass_file/7,
        fun coffee_file/7,
        fun less_file/7,
        fun template_file/7,
        fun mediaclass_file/7,
        fun po_file/7,
        fun lib_file/7,
        fun dispatch_file/7
    ].

-include_lib("zotonic_core/include/zotonic.hrl").

%% ------------------------------ Map files to actions ------------------------------

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


template_file(_Verb, _Application, {priv, <<"template">>, _Path}, <<".tpl">>, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
template_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


mediaclass_file(_Verb, _Application, {priv, <<"template">>, _Path}, <<".config">>, <<"mediaclass">>, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
mediaclass_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


po_file(_Verb, _Application, {priv, <<"translations">>, _Path}, <<".po">>, _Root, _Split, _Filename) ->
    {ok, [
        {?MODULE, reload_translations, []}
    ]};
po_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.


lib_file(_Verb, _Application, {priv, <<"lib">>, _Path}, _Ext, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reindex_modules, []}
    ]};
lib_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
    false.

dispatch_file(_Verb, _Application, {priv, <<"dispatch">>, _Path}, _Ext, _Root, _Split, Filename) ->
    {ok, [
        {?MODULE, mark_modified, [Filename]},
        {?MODULE, reload_dispatch, []}
    ]};
dispatch_file(_Verb, _Application, _What, _Ext, _Root, _Split, _Filename) ->
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

%% @doc A Zotonic template changed
%% @todo This should be handled by a module indexer, which can make a small reindex of
%%       the affected application templates.
reindex_modules() ->
    lists:foreach(
        fun(Ctx) ->
            z_module_indexer:reindex(Ctx)
        end,
        z_sites_manager:get_site_contexts()).

%% @doc Reload translations
%% @todo This should be handled incrementally, passing the changed po file.
reload_translations() ->
    lists:foreach(
        fun(Ctx) ->
            z_trans_server:load_translations(Ctx)
        end,
        z_sites_manager:get_site_contexts()).

%% @doc Reload all dispatch rules
reload_dispatch() ->
    lists:foreach(
        fun(Ctx) ->
            z_dispatcher:reload(Ctx)
        end,
        z_sites_manager:get_site_contexts()).

%% @doc Run the build command in a lib-src directory
run_build(Application, BuildCmd) ->
    zotonic_filehandler:terminal_notifier("Build: " ++ app_path(Application, BuildCmd)),
    CmdOpts = [
        {env, [
            {"APP_DIR", code:lib_dir(Application)}
        ]}
    ],
    BuildDir = filename:dirname(BuildCmd),
    BuildCmd1 = "cd " ++ z_utils:os_escape(BuildDir) ++ "; sh -c " ++ z_utils:os_escape(BuildCmd),
    zotonic_filehandler_compile:run_cmd(BuildCmd1, CmdOpts).

%% @doc Mark a file as modified in the mtime index
mark_modified(Filename) ->
    z_file_mtime:modified(Filename),
    ok.

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


%% @doc Find a build commmand on the path to the changed lib-src file.
%%      We look for the following files:
%%          - build.sh
%%          - build-watcher.sh
%%          - build-targets
%%      The build-targets file contains a list of files, relative to the "lib" directory.
%%      It is used to check if the build command should be called or not
%%      TODO: The build-watcher.sh is a background process that is started and kept running.
build_command(Application, SrcPath) ->
    LibSrcDir = filename:join([code:priv_dir(Application), "lib-src"]),
    LibDir = filename:join([code:priv_dir(Application), "lib"]),
    case find_build(LibSrcDir, filename:split(filename:dirname(SrcPath))) of
        {ok, BuildCmd} ->
            case is_build_needed(BuildCmd, LibDir, filename:join(LibSrcDir, SrcPath)) of
                true ->
                    {ok, BuildCmd};
                false ->
                    ok
            end;
        false ->
            false
    end.

find_build(_LibSrcDir, []) ->
    false;
find_build(LibSrcDir, Dir) ->
    Dirname = filename:join([LibSrcDir] ++ Dir),
    BuildCmd = filename:join(Dirname, "build.sh"),
    case filelib:is_file(BuildCmd) of
        true ->
            {ok, BuildCmd};
        false ->
            Up = lists:reverse(tl(lists:reverse(Dir))),
            find_build(LibSrcDir, Up)
    end.

is_build_needed(BuildCmd, LibDir, SrcFilename) ->
    case filelib:is_file(SrcFilename) of
        false ->
            true;
        true ->
            Targets = filename:join(filename:dirname(BuildCmd), "build-targets"),
            case file:read_file(Targets) of
                {ok, Bin} ->
                    Lines = binary:split(Bin, <<10>>, [global]),
                    Lines1 = [ z_string:trim(Line) || Line <- Lines ],
                    not lists:all(
                        fun
                            (<<>>) -> true;
                            (<<"#", _/binary>>) -> true;
                            (Tgt) ->
                                TgtFile = filename:join(LibDir, Tgt),
                                not is_newer(SrcFilename, TgtFile)
                        end,
                        Lines1);
                {error, _} ->
                    true
            end
    end.


% check_run_sitetest(Basename, F) ->
%     %% check run individual test
%     case re:run(Basename, "^((.+)_.*_sitetest).erl$", [{capture, [1, 2], list}]) of
%         {match, [Module, SiteStr]} ->
%             zotonic_filehandler_compile:ld(z_convert:to_atom(Module)),
%             z_sitetest:run(z_convert:to_atom(SiteStr), [z_convert:to_atom(Module)]);
%         nomatch ->
%             %% check whether compiled file is part of a site; if so, run its sitetests when we're watching it.
%             case re:run(F, "/sites/([^/]+).*?/", [{capture, all_but_first, list}]) of
%                 nomatch ->
%                     nop;
%                 {match, [SiteStr]} ->
%                     Module = z_convert:to_atom(filename:basename(Basename, ".erl")),
%                     zotonic_filehandler_compile:ld(z_convert:to_atom(Module)),
%                     Site = z_convert:to_atom(SiteStr),
%                     z_sitetest:is_watching(Site) andalso z_sitetest:run(Site)
%             end
%     end.
