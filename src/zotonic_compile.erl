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

-export([all/0, all/1, d/0]).

-include_lib("include/zotonic.hrl").

all() ->
    all([]).


%% @doc Does a parallel compile of the files in the different Zotonic directories.
all(Options0) ->
    Options = Options0 ++ compile_options(),
    Work = d(),
    Ref = make_ref(),
    Self = self(),

    spawn_link(fun() -> compile(Self, Ref, Work, Options) end),
    receive
        {Ref, Result} -> Result
    end.

compile(Parent, Ref, Work, Options) ->
    Workers = spawn_compile_workers(Options, self()),
    Parent ! {Ref, compile_loop(Work, Workers, ok)}.

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
            Result = make:files(filelib:wildcard(Work), Options),
            Parent ! {result, Result},
            compile_worker_process(Options, Parent);
        done ->
            Parent ! {done, self()},
            ok
    end.


compile_dirs() ->
    application:load(zotonic),
    {ok, Sites} = application:get_env(zotonic, user_sites_dir),
    {ok, Modules} = application:get_env(zotonic, user_modules_dir),
    [
     "*.erl",
     "src/*.erl",
     "src/*/*.erl",
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
     "priv/sites/*/modules/*/*/*.erl",

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
    [{i, "include"},
     {i, "deps/webzmachine/include"},
     {outdir, "ebin"},
     {parse_transform, lager_transform},
     debug_info].    


%% @doc For a list of glob patterns, split all patterns which contain
%% /*/* up in more patterns, so that we can parallelize it even more.
unglob(Patterns) ->
    lists:foldl(fun(Pattern, All) ->
                        case re:split(Pattern, "/\\*/\\*", [{return, list}, {parts, 2}]) of
                            [_] -> [Pattern | All];
                            [Start, End] ->
                                Dirs = lists:filter(fun filelib:is_dir/1, filelib:wildcard(Start ++ "/*")),
                                [Dir ++ "/*" ++ End || Dir <- Dirs] ++ All
                        end
                end,
                [],
                Patterns).

d() ->
    unglob(compile_dirs()).
