%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Node-wide barrier between builds/code activation and schema migrations.
%% Only a failed or interrupted deployment stays closed until a successful retry. Watcher
%% events are retained by the file handler while closed. All leases are monitored.
%% @end

%% Copyright 2026 Marc Worrell
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

-module(zotonic_filehandler_update).
-behaviour(gen_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("zotonic_notifier/include/zotonic_notifier.hrl").

-export([start_link/0, run/1, build/1, deploy/1, schema/1, try_idle/1, is_pending/0, status/0, publish_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    owner = undefined,
    readers = #{},
    waiting = [],
    failure = undefined,
    deployment = false
}).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run an automatic update exclusively. Errors release the barrier.
-spec run(Fun) -> Result when Fun :: fun(() -> Result), Result :: term().
run(Fun) -> run(Fun, update).

-spec build(Fun) -> Result when Fun :: fun(() -> Result), Result :: term().
build(Fun) -> run(Fun, build).

%% @doc Start a deployment. Failures remain blocked until a successful rebuild.
-spec deploy(Fun) -> Result when Fun :: fun(() -> Result), Result :: term().
deploy(Fun) -> run(Fun, deploy).

run(Fun, Kind) ->
    case get({?MODULE, schema}) of
        true -> {error, update_during_schema};
        undefined -> run_update(Fun, Kind)
    end.

run_update(Fun, Kind) ->
    case gen_server:call(?MODULE, {begin_update, Kind}, infinity) of
        ok ->
            try
                ok = zotonic_filehandler_handler:update_barrier(Kind),
                FunResult = Fun(),
                flush_deployment_changes(),
                FunResult
            of
                {error, _} = Error ->
                    ok = gen_server:call(?MODULE, {finish, Error}),
                    Error;
                Result ->
                    ok = gen_server:call(?MODULE, {finish, ok}),
                    Result
            catch Class:Reason:Stack ->
                ok = gen_server:call(?MODULE, {finish, {error, {Class, Reason}}}),
                erlang:raise(Class, Reason, Stack)
            end;
        {error, _} = Error -> Error
    end.

%% The watcher debounces notifications. Deliver its queued events while the
%% deployment barrier still marks them as asset changes that must not rebuild.
flush_deployment_changes() ->
    case {whereis(zotonic_filehandler_handler), status()} of
        {Pid, _} when Pid =:= self() -> ok;
        {_, #{deployment := true}} -> zotonic_filewatcher_handler:flush();
        _ -> ok
    end.

%% @doc Hold a read lease for the entire migration, including version recording.
-spec schema(Fun) -> Result when Fun :: fun(() -> Result), Result :: term().
schema(Fun) ->
    case get({?MODULE, schema}) of
        true -> Fun();
        undefined -> schema_lease(Fun)
    end.

schema_lease(Fun) ->
    case gen_server:call(?MODULE, schema, infinity) of
        {ok, Ref} ->
            put({?MODULE, schema}, true),
            try Fun()
            after
                erase({?MODULE, schema}),
                gen_server:call(?MODULE, {release, Ref})
            end;
        {error, _} = Error -> Error
    end.

%% @doc Run a short cache operation only when no update is pending. The lease
%% makes checking the barrier and the operation atomic with respect to new builds.
%% Unlike schema/1, this never waits for a build to complete inside a cache server.
-spec try_idle(Fun) -> {ok, Result} | {error, update_in_progress} when
    Fun :: fun(() -> Result), Result :: term().
try_idle(Fun) ->
    case gen_server:call(?MODULE, try_idle) of
        {ok, Ref} ->
            try {ok, Fun()}
            after gen_server:call(?MODULE, {release, Ref})
            end;
        {error, _} = Error -> Error
    end.

-spec is_pending() -> boolean().
is_pending() ->
    gen_server:call(?MODULE, is_pending).

%% @doc Public state for administration; never expose raw failure terms or paths.
-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status).

%% @doc Republish the current snapshot, for newly started sites or observers.
-spec publish_status() -> ok.
publish_status() -> gen_server:cast(?MODULE, publish_status).

init([]) ->
    %% Retain failures if this coordinator alone is restarted by its supervisor.
    Failure = case persistent_term:get(?MODULE, undefined) of
        undefined -> undefined;
        _ -> coordinator_restarted
    end,
    persistent_term:put(?MODULE, coordinator_started),
    self() ! publish_status,
    {ok, #state{failure = Failure}}.

handle_call(status, _From, State) ->
    {reply, public_status(State), State};
handle_call(is_pending, _From, State) ->
    {reply, State#state.owner =/= undefined orelse State#state.failure =/= undefined, State};
%% A restarted coordinator has lost its leases. A VM restart is required;
%% opening the barrier could otherwise race a migration still running elsewhere.
handle_call({begin_update, _}, _From, #state{failure = coordinator_restarted} = State) ->
    {reply, {error, coordinator_restarted}, State};
handle_call({begin_update, update}, _From, #state{failure = Failure} = State)
    when Failure =/= undefined ->
    {reply, {error, {code_update_failed, Failure}}, State};
handle_call({begin_update, Kind}, From, #state{owner = undefined, readers = Readers} = State) ->
    {Pid, _} = From,
    Ref = monitor(process, Pid),
    persistent_term:put(?MODULE, interrupted_update),
    State1 = State#state{
        owner = {Pid, Ref, From},
        deployment = Kind =:= deploy orelse State#state.deployment
    },
    notify_status(State1),
    case map_size(Readers) of
        0 ->
            gen_server:reply(From, ok),
            {noreply, State1#state{owner = {Pid, Ref, active}}};
        _ -> {noreply, State1}
    end;
handle_call({begin_update, _Kind}, _From, State) ->
    {reply, {error, update_in_progress}, State};
handle_call(try_idle, _From, #state{owner = Owner, failure = Failure} = State)
    when Owner =/= undefined; Failure =/= undefined ->
    {reply, {error, update_in_progress}, State};
handle_call(try_idle, From, State) ->
    handle_call(schema, From, State);
handle_call(schema, From, #state{owner = Owner, waiting = Waiting} = State)
    when Owner =/= undefined ->
    {Pid, _} = From,
    Ref = monitor(process, Pid),
    {noreply, State#state{waiting = Waiting ++ [{Ref, From}]}};
handle_call(schema, _From, #state{failure = Failure} = State) when Failure =/= undefined ->
    {reply, {error, {code_update_failed, Failure}}, State};
handle_call(schema, {Pid, _}, #state{readers = Readers} = State) ->
    Ref = monitor(process, Pid),
    {reply, {ok, Ref}, State#state{readers = Readers#{Ref => Pid}}};
handle_call({release, Ref}, {Pid, _}, #state{readers = Readers} = State) ->
    case maps:find(Ref, Readers) of
        {ok, Pid} ->
            demonitor(Ref, [flush]),
            {reply, ok, maybe_begin(State#state{readers = maps:remove(Ref, Readers)})};
        _ -> {reply, {error, not_owner}, State}
    end;
handle_call({finish, Result}, {Pid, _}, #state{owner = {Pid, Ref, active}} = State) ->
    demonitor(Ref, [flush]),
    State1 = finish(Result, State#state{owner = undefined}),
    notify_status(State1),
    {reply, ok, State1};
handle_call(_Message, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(publish_status, State) ->
    notify_status(State),
    {noreply, State};
handle_cast(_Message, State) -> {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, Reason}, #state{owner = {_, Ref, _}} = State) ->
    State1 = finish({error, {update_process_down, Reason}}, State#state{owner = undefined}),
    notify_status(State1),
    {noreply, State1};
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    {noreply, maybe_begin(State#state{
        readers = maps:remove(Ref, State#state.readers),
        waiting = lists:keydelete(Ref, 1, State#state.waiting)
    })};
handle_info(publish_status, State) ->
    notify_status(State),
    {noreply, State};
handle_info(_Message, State) -> {noreply, State}.

maybe_begin(#state{owner = {Pid, Ref, From}, readers = Readers} = State)
    when From =/= active, map_size(Readers) =:= 0 ->
    gen_server:reply(From, ok),
    State#state{owner = {Pid, Ref, active}};
maybe_begin(State) -> State.

finish(ok, #state{waiting = Waiting, readers = Readers} = State) ->
    persistent_term:put(?MODULE, coordinator_started),
    Readers1 = lists:foldl(fun({Ref, {Pid, _} = From}, Acc) ->
        gen_server:reply(From, {ok, Ref}),
        Acc#{Ref => Pid}
    end, Readers, Waiting),
    case whereis(zotonic_filehandler_handler) of
        undefined -> ok;
        Handler -> Handler ! replay_changes
    end,
    case whereis(z_file_mtime) of
        undefined -> ok;
        MTime -> MTime ! update_released
    end,
    State#state{failure = undefined, deployment = false, waiting = [], readers = Readers1};
finish({error, Reason}, #state{deployment = false} = State) ->
    ?LOG_ERROR(#{
        text => <<"Automatic code update failed; automatic actions remain enabled">>,
        in => zotonic_filehandler,
        result => error,
        reason => Reason
    }),
    finish(ok, State);
finish({error, Reason}, #state{waiting = Waiting} = State) ->
    ?LOG_ERROR(#{
        text => <<"Code update failed; schema upgrades blocked until a successful rebuild">>,
        in => zotonic_filehandler,
        result => error,
        reason => Reason
    }),
    persistent_term:put(?MODULE, Reason),
    lists:foreach(fun({Ref, From}) ->
        demonitor(Ref, [flush]),
        gen_server:reply(From, {error, {code_update_failed, Reason}})
    end, Waiting),
    State#state{failure = Reason, waiting = []}.

public_status(#state{owner = Owner, failure = Failure, deployment = Deployment}) ->
    #{active => Owner =/= undefined, blocked => Failure =/= undefined,
      deployment => Deployment, restart_required => Failure =:= coordinator_restarted}.

%% Synchronous delivery preserves transition order. Core owns the MQTT bridge;
%% the standalone filehandler has no dependency on sites or the MQTT runtime.
notify_status(State) ->
    try
        zotonic_notifier:notify_sync(?SYSTEM_NOTIFIER, code_update_status,
            {code_update_status, public_status(State)}, undefined)
    catch
        %% Observability must never fail a deployment, including early startup.
        _:_ -> ok
    end.
