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

-module(zotonic_compile).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-export([
    all/0,
    recompile/1,
    compile_options/1,
    ld/0, ld/1
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


%% @doc Compile all files
-spec all() -> ok.
all() ->
    Ret = case os:getenv("ZOTONIC") of
        false ->
            os:cmd("./rebar3 compile");
        ZotonicDir ->
            Cmd = [
                "cd ", z_utils:os_filename(ZotonicDir),
                "; ./rebar3 compile"
            ],
            os:cmd(lists:flatten(Cmd))
    end,
    lager:debug("rebar3 compile returns '~s'", [Ret]),
    ok.

%% @todo When "./rebar3 compile" runs, then all .../test/*.erl file are touched.
%%       This forces a recompile of all these files. We might not want to
%%       automatically recompile these files during a rebar3 run. Of course
%%       problem is to known if rebar3 is running...
-spec recompile(file:filename_all()) -> ok | error.
recompile(File) ->
    case compile_options(File) of
        {ok, Options} ->
            lager:debug("Recompiling '~s' using make", [File]),
            case make:files([File], Options) of
                up_to_date -> ok;
                Other -> Other
            end;
        false ->
            % Might be some new OTP app, recompile with rebar3
            % Output can be anything ... no error checking for now :(
            lager:debug("Recompile all files due to '~s'", [File]),
            _ = all(),
            ok
    end.

-spec compile_options(file:filename_all()) -> {ok, list()} | false.
compile_options(Filename) ->
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
