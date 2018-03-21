%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014-2017 Arjan Scherpenisse
%%
%% @doc Compilation of Zotonic files

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

-module(zotonic_filehandler_compile).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-export([
    all/0,
    recompile/1,
    compile_options/1,
    run_cmd/1,
    run_cmd/2,
    ld/0,
    ld/1,
    code_path_check/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc Load all changed beam files, return list of reloaded modules.
-spec ld() -> list( code:load_ret() ).
ld() ->
    Ms = reloader:all_changed(),
    [ ld(M) || M <- Ms ].

%% @doc Load a specific beam file, reattach any observers etc.
-spec ld(module()) -> code:load_ret().
ld(Module) when is_atom(Module) ->
    code:purge(Module),
    case code:load_file(Module) of
        {error, _} = Error ->
            lager:error("Error loading module ~p: ~p",
                        [Module, Error]),
            Error;
        {module, _} = Ok ->
            z_sites_manager:module_loaded(Module),
            Ok
    end.

%% @doc Check if the application is present in the current code path
-spec code_path_check( binary() ) -> boolean().
code_path_check(AppFilename) ->
    Path = filename:dirname( filename:dirname( AppFilename )),
    CurrentAppVsn = code_path_to_appvsn(),
    case maybe_add_path(Path, CurrentAppVsn) of
        {true, AppVsn} ->
            zotonic_filewatcher_sup:restart_watchers(),
            zotonic_filehandler:terminal_notifier("New application: "++z_convert:to_list(AppVsn)),
            true;
        false ->
            % App (with this vsn) exists in the code path
            false
    end.

%% @doc List all applications (with opt version number) as binaries.
-spec code_path_to_appvsn() -> list( binary() ).
code_path_to_appvsn() ->
    lists:map(
        fun(Path) ->
            z_convert:to_binary(filename:basename(filename:dirname(Path)))
        end,
        code:get_path()).

%% @doc Check if a new application (with optional vsn) directory needs to be added to the code path.
%%      Return 'true' if the code path has been changed.
-spec maybe_add_path( file:filename_all(), list( binary )) -> boolean().
maybe_add_path(AppDir, Paths) ->
    AppVsn = z_convert:to_binary(filename:basename(AppDir)),
    case lists:member(AppVsn, Paths) of
        true ->
            false;
        false ->
            AppBeam = filename:join([ AppDir, "ebin" ]),
            code:add_patha(unicode:characters_to_list(AppBeam)),
            {true, AppVsn}
    end.


%% @doc Compile all files
-spec all() -> ok.
all() ->
    Cmd = case os:getenv("ZOTONIC") of
        false ->
            "./rebar3 compile";
        ZotonicDir ->
            lists:flatten([
                "cd ", z_utils:os_filename(ZotonicDir),
                "; ./rebar3 compile"
            ])
    end,
    zotonic_filehandler:terminal_notifier("Compile all"),
    run_cmd(Cmd).

run_cmd(Cmd) ->
    run_cmd(Cmd, []).

run_cmd(Cmd, Opts) when is_binary(Cmd) ->
    run_cmd(unicode:characters_to_list(Cmd, utf8), Opts);
run_cmd(Cmd, Opts) ->
    case exec:run(lists:flatten(Cmd), [ sync, stdout, stderr ] ++ Opts) of
        {ok, Out} ->
            StdErr = proplists:get_value(stderr, Out, []),
            case StdErr of
                [] ->
                    ok;
                StdErr ->
                    lager:error("Running '~s' returned '~s'", [Cmd, iolist_to_binary(StdErr)]),
                    ok
            end;
        {error, Args} = Error when is_list(Args) ->
            StdErr = proplists:get_value(stderr, Args, []),
            StdOut = proplists:get_value(stdout, Args, []),
            case {StdErr, StdOut} of
                {[], []} ->
                    lager:error("Error running '~s': ~p", [Cmd, Error]);
                {StdErr, _} when StdErr =/= [] ->
                    lager:error("Error running '~s':~n~s", [Cmd, iolist_to_binary(StdErr)]);
                {_, StdOut} ->
                    lager:error("Error running '~s':~n~s", [Cmd, iolist_to_binary(StdOut)])
            end,
            Error
    end.

%% @todo When "./rebar3 compile" runs, then all .../test/*.erl file are touched.
%%       This forces a recompile of all these files. We might not want to
%%       automatically recompile these files during a rebar3 run. Of course
%%       problem is to known if rebar3 is running...
-spec recompile(file:filename_all()) -> ok | {error, term()}.
recompile(File) when is_binary(File) ->
    recompile(unicode:characters_to_list(File, utf8));
recompile(File) ->
    case compile_options(File) of
        {ok, Options} ->
            lager:debug("Recompiling '~s' using make", [File]),
            zotonic_filehandler:terminal_notifier("Compiling: " ++ filename:basename(File)),
            case make:files([File], Options) of
                up_to_date -> ok;
                Other -> {error, Other}
            end;
        false ->
            % Might be some new OTP app, recompile with rebar3
            % Output can be anything ... no error checking for now :(
            lager:debug("Recompile all files due to '~s'", [File]),
            all()
    end.

-spec compile_options(file:filename_all()) -> {ok, list()} | false.
compile_options(File) when is_binary(File) ->
    compile_options(unicode:characters_to_list(File, utf8));
compile_options(Filename) ->
    case compile_options_1(Filename) of
        {ok, Options} ->
            Options1 = [ Opt || Opt <- Options, Opt =/= error_summary ],
            {ok, Options1};
        {error, _} = Error ->
            Error
    end.

compile_options_1(Filename) ->
    case previous_compile_options(Filename) of
        false ->
            % Guess the options based on other ebin files
            guess_compile_options(Filename);
        {ok, _} = Ok ->
            Ok
    end.

guess_compile_options(Filename) ->
    case guess_compile_options_src(Filename) of
        {ok, Options} -> {ok, Options};
        false -> guess_compile_options_ebin(Filename)
    end.

guess_compile_options_src(Filename) ->
    SrcFiles = filelib:wildcard(filename:join(filename:dirname(Filename), "*.erl")),
    first(fun previous_compile_options/1, SrcFiles).

first(_F, []) -> false;
first(F, [A|As]) ->
    case F(A) of
        false -> first(F, As);
        {ok, _} = Ok -> Ok
    end.

guess_compile_options_ebin(Filename) ->
    EbinDir = ebin_path(Filename),
    case filelib:wildcard(filename:join(EbinDir, "*.beam")) of
        [] -> false;
        BeamFiles -> previous_compile_options_any(BeamFiles)
    end.

ebin_path(SrcFile) ->
    Parts = filename:split(SrcFile),
    SrcPath = lists:takewhile(fun is_not_src/1, Parts),
    Dir = case SrcPath of
        Parts -> filename:dirname(SrcFile);
        [] -> [];
        Path1 -> filename:join(Path1)
    end,
    EbinDir = filename:join([Dir, "ebin"]),
    case filelib:is_dir(EbinDir) of
        true ->
            EbinDir;
        false ->
            % Could be that we are in _checkouts or in apps
            % Check the name of module/lib which is one level
            % above the src dir.
            case SrcPath of
                Parts -> EbinDir;
                [] -> EbinDir;
                _ ->
                    AppName = lists:last(SrcPath),
                    case code:priv_dir(z_convert:to_list(AppName)) of
                        {error, bad_name} ->
                            filename:join([z_path:build_lib_dir(), AppName, "ebin"]);
                        PrivDir ->
                            filename:join(
                                filename:dirname(PrivDir),
                                "ebin")
                    end
            end
    end.

is_not_src("src") -> false;
is_not_src(<<"src">>) -> false;
is_not_src(_) -> true.

previous_compile_options(Filename) when is_list(Filename); is_binary(Filename) ->
    previous_compile_options(z_convert:to_atom(filename:rootname(filename:basename(Filename))));
previous_compile_options(Module) when is_atom(Module) ->
    try
        MInfo = Module:module_info(compile),
        case proplists:get_value(options, MInfo) of
            undefined -> false;
            Options -> {ok, with_outdir(Options, Module)}
        end
    catch
        error:undef -> false
    end.

previous_compile_options_any([]) -> false;
previous_compile_options_any([BeamFile|BeamFiles]) ->
    case previous_compile_options(BeamFile) of
        false -> previous_compile_options_any(BeamFiles);
        {ok, Options} -> {ok, Options}
    end.

%% @doc Add outdir if it's missing
%%      This seems to be the case after rebar3's first compile.
with_outdir(Options, Module) ->
    case proplists:lookup(outdir, Options) of
        none ->
            Outdir = filename:dirname(code:which(Module)),
            [{outdir, Outdir} | Options];
        _ ->
            Options
    end.

% zotonic_ebin_dir() ->
%     filename:join([build_lib_dir(), "zotonic_core", "ebin"]).

% zotonic_subdir(Path) when is_list(Path) ->
%     case os:getenv("ZOTONIC") of
%         false -> filename:join([file:get_cwd() | Path]);
%         ZotonicDir -> filename:join([ZotonicDir | Path])
%     end.
