%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2024 Marc Worrell
%% @doc The supervisor for websocket requests and other transient processes.
%% @end

%% Copyright 2024 Marc Worrell
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
    start/4
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
