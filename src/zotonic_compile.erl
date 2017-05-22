%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014, 2016 Arjan Scherpenisse
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
    all/0, all/1,
    recompile/1,
    compile_options/1,
    compile_options/0,
    compile_user_options/0,
    ld/0, ld/1
]).

-include_lib("zotonic.hrl").

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
all() ->
    all([]).

%% @doc Does a parallel compile of the files in the different Zotonic directories.
all(Options0) ->
    case compile_stage(zotonic, Options0) of
        ok -> compile_stage(user, Options0);
        error -> error
    end.

-spec recompile(file:filename_all()) -> up_to_date | error.
recompile(File) ->
    make:files([File], compile_options(File)).

-spec compile_options(filename:filename_all()) -> list().
compile_options(Filename) ->
    case previous_compile_options(Filename) of
        undefined ->
            % Probably a new file, guess if it is a 'user', 'zotonic' or 'deps' file
            % For deps files we compile with similar options as a beam in the closest 'ebin'
            guess_compile_options(Filename);
        Options ->
            Options
    end.

guess_compile_options(Filename) ->
    case is_zotonic_core_file(Filename) of
        true ->
            {ok, compile_options()};
        false ->
            case is_zotonic_user_file(Filename) of
                true -> {ok, compile_user_options()};
                false -> guess_compile_options_ebin(Filename)
            end
    end.

guess_compile_options_ebin(Filename) ->
    case find_ebin_on_path(filename:dirname(Filename)) of
        {ok, Dir} ->
            case filelib:wildcard(filename:join(Dir, "*.beam")) of
                [] -> {ok, compile_options()};
                BeamFiles ->
                    case previous_compile_options_any(BeamFiles) of
                        undefined -> {ok, compile_options()};
                        Options -> {ok, Options}
                    end
            end;
        {error, _} ->
            % No ebin dir on the path, assume it is a new user file
            % TODO: need to check if typical zotonic-directories/files
            %       are on the path.
            {ok, compile_user_options()}
    end.

find_ebin_on_path(Path) ->
    Ebin = filename:join(Path, "ebin"),
    case filelib:is_dir(Ebin) of
        true -> {ok, Ebin};
        false ->
            case filename:dirname(Path) of
                Path -> {error, notfound};
                Path1 -> find_ebin_on_path(Path1)
            end
    end.

-spec is_matching(Path::list(string()), Patterns::list(string())) -> boolean().
is_matching(_Parts, []) -> false;
is_matching(Parts, [Path|Rest]) ->
    PathParts = lists:reverse(filename:split(Path)),
    case is_matching_1(tl(Parts), tl(PathParts)) of
        true -> true;
        false -> is_matching(Parts, Rest)
    end.

is_zotonic_core_file(Filename) ->
    Parts = lists:reverse(filename:split(z_convert:to_list(Filename))),
    Dirs = filelib:wildcard("src/*"),
    Dirs1 = lists:filter(fun filelib:is_dir/1, Dirs),
    Dirs2 = [ filename:join(D, "*.erl") || D <- Dirs1, hd(D) =/= $. ],
    ZotonicDirs = Dirs2 ++ compile_zotonic_dirs(),
    is_matching(Parts, ZotonicDirs).

is_zotonic_user_file(Filename) ->
    Parts = lists:reverse(filename:split(z_convert:to_list(Filename))),
    is_matching(Parts, compile_user_dirs()).


is_matching_1(_, []) -> true;
is_matching_1([A|As], [A|Bs]) -> is_matching_1(As, Bs);
is_matching_1([_|As], ["*"|Bs]) -> is_matching_1(As, Bs);
is_matching_1(["ext_" ++ _|As], ["ext_*"|Bs]) -> is_matching_1(As, Bs);
is_matching_1(_, _) -> false.

previous_compile_options(Filename) ->
    Module = z_convert:to_atom(filename:rootname(filename:basename(Filename))),
    try
        MInfo = Module:module_info(compile),
        proplists:get_value(options, MInfo)
    catch
        error:undef -> undefined
    end.

previous_compile_options_any([]) -> undefined;
previous_compile_options_any([BeamFile|BeamFiles]) ->
    case previous_compile_options(BeamFile) of
        undefined -> previous_compile_options_any(BeamFiles);
        Options -> Options
    end.

compile_stage(zotonic, Options0) ->
    Options = Options0 ++ compile_options(),
    Work = unglob(compile_zotonic_dirs()),
    compile(Options, Work);
compile_stage(user, Options0) ->
    Options = Options0 ++ compile_user_options(),
    Work = unglob(compile_user_dirs()),
    compile(Options, Work).

compile(Options, Work) ->
    Workers = spawn_compile_workers(Options, self()),
    Result = compile_loop(Work, Workers, up_to_date),
    z:ld(),
    Result.

%% No work, and no workers, we are done.
compile_loop([], [], Result) ->
    case Result of
        up_to_date -> ok;
        error -> error
    end;

%% Error found, stop all workers.
compile_loop(_WorkQueue, Workers, error) ->
    [exit(Worker, normal) || Worker <- Workers],
    error;

%% No more work, but workers are still busy compiling.
compile_loop([], Workers, Result) ->
    receive
        {want_file, Worker} ->
            Worker ! done,
            compile_loop([], Workers, Result);
        {done, Worker} ->
            compile_loop([], lists:delete(Worker, Workers), Result);
        {result, R} ->
            compile_loop([], Workers, R)
    end;

%% Hand out work to the workers.
compile_loop([Work|Rest]=WorkQueue, Workers, Result) ->
    receive
        {want_file, Worker} ->
            Worker ! {work, Work},
            compile_loop(Rest, Workers, Result);
        {result, R} ->
            compile_loop(WorkQueue, Workers, R)
    end.

spawn_compile_workers(Options, Parent) ->
    NoWorkers = min(8, erlang:system_info(schedulers_online)*2),
    [spawn_link(fun() -> compile_worker_process(Options, Parent) end) || _ <- lists:seq(1, NoWorkers)].

compile_worker_process(Options, Parent) ->
    Parent ! {want_file, self()},
    receive
        {work, Work} ->
            Result = make:files(z_utils:wildcard(Work), Options),
            Parent ! {result, Result},
            compile_worker_process(Options, Parent);
        done ->
            Parent ! {done, self()},
            ok
    end.

compile_zotonic_dirs() ->
    [
     "modules/*/*.erl",
     "modules/*/*/*.erl",

     %% zotonic extensions
     "priv/extensions/ext_*/*.erl",
     "priv/extensions/ext_*/*/*.erl",

     %% builtin sites
     "priv/sites/*/*.erl",
     "priv/sites/*/*/*.erl",
     "priv/sites/*/modules/*/*.erl",
     "priv/sites/*/modules/*/*/*.erl"
    ].

compile_user_dirs() ->
    application:load(zotonic),
    Modules = user_modules_dir(),
    Sites = user_sites_dir(),
    [
     %% External modules
     Modules ++ "/*/*.erl",
     Modules ++ "/*/*/*.erl",

     %% External sites
     Sites ++ "/*/*.erl",
     Sites ++ "/*/*/*.erl",
     Sites ++ "/*/modules/*/*.erl",
     Sites ++ "/*/modules/*/*/*.erl"
    ].


%% @doc Default compile options for Zotonic Erlang files.
compile_options() ->
    _ = application:load(zotonic),
    [
        {i, zotonic_subdir(["include"])},
        {i, zotonic_subdir(["modules", "*", "include"])},
        {outdir, zotonic_ebin_dir()},
        {parse_transform, lager_transform},
        nowarn_deprecated_type,
        debug_info,
        {d, namespaced_dicts}      % OTP 17+
    ].

%% @doc Default compile options for user (sites) files that need to stay separate from the
%%      Zotonic core files.
compile_user_options() ->
    _ = application:load(zotonic),
    Outdir = application:get_env(zotonic, user_ebin_dir, zotonic_ebin_dir()),
    [
        {i, user_modules_dir()},
        {i, user_sites_dir()},
        {outdir, Outdir}
        | proplists:delete(outdir, compile_options())
    ].

zotonic_ebin_dir() ->
     zotonic_subdir(["_build", "default", "lib", "zotonic", "ebin"]).

user_modules_dir() ->
    application:get_env(zotonic, user_modules_dir, zotonic_subdir(["user", "modules"])).

user_sites_dir() ->
    application:get_env(zotonic, user_sites_dir, zotonic_subdir(["user", "sites"])).

zotonic_subdir(Path) when is_list(Path) ->
    filename:join([os:getenv("ZOTONIC") | Path]).

%% @doc For a list of glob patterns, split all patterns which contain
%% /*/* up in more patterns, so that we can parallelize it even more.
unglob(Patterns) ->
    lists:foldl(fun(Pattern, All) ->
                        case re:split(Pattern, "/\\*/\\*", [{return, list}, {parts, 2}]) of
                            [_] -> [Pattern | All];
                            [Start, End] ->
                                Dirs = lists:filter(fun filelib:is_dir/1, z_utils:wildcard(Start ++ "/*")),
                                [Dir ++ "/*" ++ End || Dir <- Dirs] ++ All
                        end
                end,
                [],
                Patterns).
