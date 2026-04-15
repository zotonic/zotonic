%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024-2026 Marc Worrell
%% @doc The supervisor for websocket requests and other transient processes.
%% @end

%% Copyright 2024-2026 Marc Worrell
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

-module(z_sidejob).

-export([
    init/0,
    limit/0,
    space/0,
    usage/0,
    start/1,
    start/3,
    start/2,
    start/4,
    start_site_unique/3,
    start_site_unique/5,
    start_system_unique/2,
    start_system_unique/4,
    run_unique/2,
    run_unique/3
]).


%% @doc Start the Zotonic sidejob supervisor
-spec init() -> supervisor:startchild_ret().
init() ->
    sidejob:new_resource(zotonic_sidejobs, sidejob_supervisor, limit()).

%% @doc Return the configured limit of sidejobs.
-spec limit() -> non_neg_integer().
limit() ->
    z_config:get(sidejobs_limit).

%% @doc Return how many processes we can still add.
-spec space() -> integer().
space() ->
    limit() - usage().

%% @doc Return the amount of running processes.
-spec usage() -> non_neg_integer().
usage() ->
    length(sidejob_supervisor:which_children(zotonic_sidejobs)).

%% @doc Start a sidejob. There is a limit on the number of sidejobs, so do check the return value.
-spec start(MFA) -> {ok, pid()} | {error, Reason} when
    MFA :: {module(), atom(), list()},
    Reason :: overload.
start({M, F, A}) ->
    start(M, F, A).

%% @doc Start a sidejob. There is a limit on the number of sidejobs, so do check the return value.
-spec start(Module, Function, Args) -> {ok, pid()} | {error, Reason} when
    Module :: module(),
    Function :: atom(),
    Args :: list(),
    Reason :: overload.
start(Module, Function, Args) ->
    sidejob_supervisor:spawn(
            zotonic_sidejobs,
            {Module, Function, Args}).

%% @doc Start a sidejob. There is a limit on the number of sidejobs, so do check the return value.
%% The Context is pruned for async calls and appended to the argument list.
-spec start(MFA, Context) -> {ok, pid()} | {error, Reason} when
    MFA :: {module(), atom(), list()},
    Context :: z:context(),
    Reason :: overload.
start({M, F, A}, Context) ->
    start(M, F, A, Context).

%% @doc Start a sidejob. There is a limit on the number of sidejobs, so do check the return value.
%% The Context is pruned for async calls and appended to the argument list.
-spec start(Module, Function, Args, Context) -> {ok, pid()} | {error, Reason} when
    Module :: module(),
    Function :: atom(),
    Args :: list(),
    Context :: z:context(),
    Reason :: overload.
start(Module, Function, Args, Context) ->
    ContextAsync = z_context:prune_for_async(Context),
    sidejob_supervisor:spawn(
            zotonic_sidejobs,
            {Module, Function, Args ++ [ ContextAsync ]}).

%% @doc Start a sidejob unique for a site. If a same-named sidejob is already running
%% for this site then the job is skipped.
-spec start_site_unique(Name, MFA, Context) -> {ok, pid()} | {error, Reason} when
    Name :: atom(),
    MFA :: {module(), atom(), list()},
    Context :: z:context(),
    Reason :: overload | already_running.
start_site_unique(Name, {M, F, A}, Context) ->
    start_site_unique(Name, M, F, A, Context).

-spec start_site_unique(Name, Module, Function, Args, Context) -> {ok, pid()} | {error, Reason} when
    Name :: atom(),
    Module :: module(),
    Function :: atom(),
    Args :: list(),
    Context :: z:context(),
    Reason :: overload | already_running.
start_site_unique(Name, Module, Function, Args, Context) ->
    RegName = site_unique_name(Name, Context),
    start_unique(RegName, Module, Function, Args, Context).

%% @doc Start a sidejob unique for the whole Erlang system. If a same-named sidejob
%% is already running then the job is skipped.
-spec start_system_unique(Name, MFA) -> {ok, pid()} | {error, Reason} when
    Name :: atom(),
    MFA :: {module(), atom(), list()},
    Reason :: overload | already_running.
start_system_unique(Name, {M, F, A}) ->
    start_system_unique(Name, M, F, A).

-spec start_system_unique(Name, Module, Function, Args) -> {ok, pid()} | {error, Reason} when
    Name :: atom(),
    Module :: module(),
    Function :: atom(),
    Args :: list(),
    Reason :: overload | already_running.
start_system_unique(Name, Module, Function, Args) ->
    RegName = system_unique_name(Name),
    start_unique(RegName, Module, Function, Args).

-spec run_unique(atom(), {module(), atom(), list()}) -> ok.
run_unique(RegName, {Module, Function, Args}) ->
    try
        true = erlang:register(RegName, self()),
        try
            erlang:apply(Module, Function, Args),
            ok
        after
            catch erlang:unregister(RegName)
        end
    catch
        error:badarg ->
            ok
    end.

-spec run_unique(atom(), {module(), atom(), list()}, z:context()) -> ok.
run_unique(RegName, {Module, Function, Args}, Context) ->
    try
        true = erlang:register(RegName, self()),
        try
            erlang:apply(Module, Function, Args ++ [ Context ]),
            ok
        after
            catch erlang:unregister(RegName)
        end
    catch
        error:badarg ->
            ok
    end.

-spec run_unique_started(pid(), reference(), atom(), {module(), atom(), list()}) -> ok.
run_unique_started(Caller, Ref, RegName, {Module, Function, Args}) ->
    try
        true = erlang:register(RegName, self()),
        Caller ! {Ref, {ok, self()}},
        try
            erlang:apply(Module, Function, Args),
            ok
        after
            catch erlang:unregister(RegName)
        end
    catch
        error:badarg ->
            Caller ! {Ref, {error, already_running}},
            ok
    end.

-spec run_unique_started(pid(), reference(), atom(), {module(), atom(), list()}, z:context()) -> ok.
run_unique_started(Caller, Ref, RegName, {Module, Function, Args}, Context) ->
    try
        true = erlang:register(RegName, self()),
        Caller ! {Ref, {ok, self()}},
        try
            erlang:apply(Module, Function, Args ++ [ Context ]),
            ok
        after
            catch erlang:unregister(RegName)
        end
    catch
        error:badarg ->
            Caller ! {Ref, {error, already_running}},
            ok
    end.

start_unique(RegName, Module, Function, Args) ->
    Ref = make_ref(),
    case start(?MODULE, run_unique_started, [self(), Ref, RegName, {Module, Function, Args}]) of
        {ok, Pid} ->
            receive
                {Ref, {ok, Pid}} ->
                    {ok, Pid};
                {Ref, {error, already_running}} ->
                    {error, already_running}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

start_unique(RegName, Module, Function, Args, Context) ->
    Ref = make_ref(),
    case start(?MODULE, run_unique_started, [self(), Ref, RegName, {Module, Function, Args}], Context) of
        {ok, Pid} ->
            receive
                {Ref, {ok, Pid}} ->
                    {ok, Pid};
                {Ref, {error, already_running}} ->
                    {error, already_running}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

site_unique_name(Name, Context) ->
    z_utils:name_for_site(unique_name(Name), Context).

system_unique_name(Name) ->
    unique_name(Name).

unique_name(Name) ->
    binary_to_atom(<<"sidejob_unique$", (z_convert:to_binary(Name))/binary>>, utf8).
