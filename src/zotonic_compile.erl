%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%%
%% @doc Compilation of Zotonic files

%% Copyright 2014 Arjan Scherpenisse
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

-module(zotonic_compile).
-author("Arjan Scherpenisse <arjan@miraclethings.nl>").

-export([
    all/0, all/1,
    compile_options/0,
    ld/0, ld/1
]).

-include_lib("zotonic.hrl").

%% @doc Load all changed beam files
ld() ->
    Ms = reloader:all_changed(),
    [ ld(M) || M <- Ms ].

%% @doc Load a specific beam file, reattach any observers etc.
ld(Module) when is_atom(Module) ->
    code:purge(Module),
    Ret = code:load_file(Module),
    z_sites_manager:module_loaded(Module),
    Ret.


%% @doc Compile all files
all() ->
    all([]).

%% @doc Does a parallel compile of the files in the different Zotonic directories.
all(Options0) ->
    case compile_stage(zotonic, Options0) of
        ok -> compile_stage(user, Options0);
        error -> error
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
    Ref = make_ref(),
    Self = self(),

    spawn_link(fun() -> compile(Self, Ref, Work, Options) end),
    receive
        {Ref, Result} ->
            z:ld(),
            Result
    end.

compile(Parent, Ref, Work, Options) ->
    Workers = spawn_compile_workers(Options, self()),
    Parent ! {Ref, compile_loop(Work, Workers, up_to_date)}.

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

compile_dirs() ->
    compile_zotonic_dirs() ++ compile_user_dirs().

compile_zotonic_dirs() ->
    [
     "*.erl",
     "src/*.erl",
     "src/*/*.erl",
     "src/tests/erlydtl/*.erl",
     "modules/*/*.erl",
     "modules/*/*/*.erl",
     "modules/*/deps/*/src/*.erl",
     "priv/extensions/ext_*/*.erl",
     "priv/extensions/ext_*/*/*.erl",
     %% legacy external modules location
     "priv/modules/*/*.erl",
     "priv/modules/*/*/*.erl",

     %% builtin sites
     "priv/sites/*/*.erl",
     "priv/sites/*/*/*.erl",
     "priv/sites/*/modules/*/*.erl",
     "priv/sites/*/modules/*/*/*.erl"
    ].

compile_user_dirs() ->
    application:load(zotonic),
    {ok, Modules} = application:get_env(zotonic, user_modules_dir),
    {ok, Sites} = application:get_env(zotonic, user_sites_dir),
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


compile_options() ->
    application:load(zotonic),
    {ok, Sites} = application:get_env(zotonic, user_sites_dir),
    {ok, Modules} = application:get_env(zotonic, user_modules_dir),
    [{i, "include"},
     {i, "deps/webzmachine/include"},
     {i, Modules},
     {i, Sites},
     {outdir, "ebin"},
     {parse_transform, lager_transform},
     nowarn_deprecated_type,
     debug_info] ++ platform_defines_r17up().

compile_user_options() ->
    application:load(zotonic),
    Outdir = application:get_env(zotonic, user_ebin_dir, "ebin"),
    [ {outdir, Outdir} | compile_options()].

platform_defines_r17up() ->
    case re:run(erlang:system_info(otp_release), "^[0-9].*") of
        {match, _} ->
            [{d, namespaced_dicts}];
        nomatch ->
            []
    end.

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
