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

    flush_messages(),
    Workers = spawn_compile_workers(Options, self()),

    case serve_queue(unglob(compile_dirs())) of
        error ->
            killall(Workers),
            error;
        ok ->
            case wait_for_results(Workers) of
                up_to_date -> ok;
                error -> error
            end
    end.

spawn_compile_workers(Options, Parent) ->
    NoWorkers = min(8, erlang:system_info(schedulers_online)*2),
    [spawn_link(fun() -> compile_worker_process(Options, Parent) end) || _ <- lists:seq(1, NoWorkers)].


serve_queue(Queue) ->
    serve_queue(Queue, ok).

serve_queue(_, error) ->
    error;
serve_queue([], _) ->
    ok;
serve_queue([Work | Rest]=WorkQueue, Result) ->
    receive
        {want_file, Worker} ->
            Worker ! {work, Work},
            serve_queue(Rest, Result);
        {result, R} ->
            serve_queue(WorkQueue, R)
    end.


wait_for_results(Workers) ->
    wait_for_results(Workers, ok).

wait_for_results([], Result) ->
    Result;
wait_for_results(Workers, error) ->
    killall(Workers),
    error;
wait_for_results(Workers, Result) ->
    receive 
        {want_file, Worker} ->
            Worker ! done,
            wait_for_results(Workers, Result);
        {done, Worker} ->
            wait_for_results(lists:delete(Worker, Workers), Result);
        {result, R} ->
            wait_for_results(Workers, R)
    end.

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


killall(Pids) ->
    [exit(Pid, normal) || Pid <- Pids].


flush_messages() ->
    receive 
        {want_file, _} -> flush_messages();
        {done, _} -> flush_messages();
        {result, _} -> flush_messages()
    after 0 ->
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
