%% @copyright 2026 Marc Worrell
%% @doc Initialize missing resource defaults and migrate stored properties in resumable batches.
%% @end

%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%     http://www.apache.org/licenses/LICENSE-2.0
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_rsc_defaults).

-export([start/1, suspend/1, invalidate/1, status/1, needed/1, prepare/4, backfill/1, backfill/2, rebuild/2]).

-include_lib("zotonic.hrl").

% Serialize replacement of the rebuild task, not resource reads or writes.
-define(REBUILD_QUEUE_LOCK, 1920164964).

%% @doc Start/resume only after all fetch observers have been registered.
-spec start(z:context()) -> ok.
start(Context) ->
    case available(Context) andalso modules_ready(Context) of
        true ->
            % Preserve the pending migration cursor across ordinary site restarts.
            KeepCursor = fun
                (undefined, undefined, Due, _Ctx) -> {ok, {Due, []}};
                (Due, Args, _NewDue, _Ctx) -> {ok, {Due, Args}}
            end,
            {ok, _} = z_pivot_rsc:insert_task(?MODULE, backfill, <<>>, KeepCursor, Context),
            persistent_term:put({?MODULE, z_context:site(Context)}, false),
            ok;
        false -> ok
    end.

%% @doc Pause resolution while observers change; keep all stored privacy values.
-spec suspend(z:context()) -> ok.
suspend(Context) ->
    persistent_term:put({?MODULE, z_context:site(Context)}, true),
    ok.

available(Context) ->
    z_db:has_connection(Context)
    andalso lists:member(privacy, z_db:column_names(rsc, Context)).

paused(Context) ->
    persistent_term:get({?MODULE, z_context:site(Context)}, true).

%% Read the manager-owned flag without database queries or synchronous calls.
modules_ready(Context) ->
    case z_module_manager:all_running(Context) of
        true -> true;
        false ->
            log_delay(modules_not_running, Context),
            false
    end.

migration_ready(Context) ->
    case modules_ready(Context) of
        false -> false;
        true ->
            case paused(Context) of
                true -> log_delay(defaults_suspended, Context), false;
                false -> true
            end
    end.

log_delay(Reason, Context) ->
    ?LOG_NOTICE(#{text => <<"Delaying resource properties migration">>,
        in => zotonic_core, site => z_context:site(Context), reason => Reason}).

%% @doc Queue a manual sweep of resources with missing defaults or legacy properties.
-spec invalidate(z:context()) -> ok.
invalidate(Context) ->
    case available(Context) of
        true ->
            z_db:transaction(fun(Ctx) ->
                ok = z_migration:batch_lock(Ctx),
                z_db:q("select pg_advisory_xact_lock(hashtext(current_schema()), $1)",
                    [?REBUILD_QUEUE_LOCK], Ctx),
                % Allocate a new task ID. An already-running old task can then only
                % update/delete its own ID, never overwrite this newer rebuild.
                z_db:q("delete from pivot_task_queue where module = $1 and function = $2 and key = ''",
                    [?MODULE, rebuild], Ctx),
                {ok, _} = z_db:insert(pivot_task_queue, #{
                    <<"module">> => ?MODULE,
                    <<"function">> => rebuild,
                    <<"key">> => <<>>,
                    <<"args">> => [0]
                }, Ctx),
                ok
            end, Context);
        false -> ok
    end.

%% @doc Report pending data and queued/active work without counting resources.
-spec status(z:context()) -> map().
status(Context) ->
    Needed = needed(Context),
    {ok, Tasks} = z_db:qmap_props("select id, function, props from pivot_task_queue "
        "where module = 'z_rsc_defaults' and function in ('backfill','rebuild')", Context),
    ActiveId = case z_pivot_rsc:status(Context) of
        {ok, #{task_id := Id, task_pid := Pid}} when is_pid(Pid) -> Id;
        _ -> undefined
    end,
    Running = lists:any(fun
        (#{<<"function">> := <<"rebuild">>}) -> true;
        (#{<<"id">> := Id}) when Id =:= ActiveId -> true;
        (#{<<"args">> := [After]}) when After > 0 -> true;
        (_) -> false
    end, Tasks),
    #{id => <<"rsc_defaults">>, title => ?__("Resource properties", Context),
      description => ?__("Migrate stored resource properties and module defaults.", Context),
      is_needed => Needed, is_running => Running, can_start => true,
      url => z_dispatcher:url_for(admin_status, Context)}.

%% @doc Indexed existence check for all pending resource-default conversions.
-spec needed(z:context()) -> boolean().
needed(Context) ->
    z_db:q1("select exists(select 1 from rsc where privacy = -1 or props_json is null or content_group_id is null)", Context).

%% @doc Resolve only stored defaults; never persist unrelated computed fetch properties.
%% The caller holds the resource row lock.
-spec prepare(integer(), map(), map(), z:context()) -> map().
prepare(Id, Changes, Raw, Context) ->
    prepare(Id, Changes, Raw, false, Context).

prepare(Id, Changes, Raw, IsMigration, Context) ->
    Privacy = maps:get(<<"privacy">>, Changes, original(Id, Raw, Context)),
    Input = (maps:merge(Raw, Changes))#{<<"id">> => Id, <<"privacy">> => Privacy},
    IsPaused = paused(Context),
    Resolved = case IsPaused of
        true -> Input;
        false -> z_notifier:foldr(#rsc_get_raw{id = Id}, Input, z_context:set(rsc_defaults, true, Context))
    end,
    Value = case maps:get(<<"privacy">>, Resolved, undefined) of
        N when is_integer(N), N >= 0, N =< 2147483647 -> N;
        undefined when not IsPaused -> 0; % ACL modules without a privacy concept.
        _ -> -1
    end,
    Fixed = case IsMigration of
        true -> maps:merge(Changes, migration_changes(Input, Resolved, Context));
        false -> Changes
    end,
    Defaults = Fixed#{<<"privacy">> => Value},
    WithGroup = case maps:get(<<"content_group_id">>, Input, undefined) of
        undefined ->
            case maps:get(<<"content_group_id">>, Resolved, undefined) of
                CG when is_integer(CG) -> Defaults#{<<"content_group_id">> => CG};
                _ -> Defaults
            end;
        _ -> Defaults
    end,
    %% Force legacy conversion even if this update changes only physical columns.
    %% z_db merges JSON over props, removes physical columns and clears props atomically.
    WithGroup#{<<"props_json">> => maps:get(<<"props_json">>, Changes, #{})}.

%% Persist rsc_get_raw field fixes during migration, including removals. An undefined
%% value tells z_db to remove a property from the merged legacy/JSON properties.
%% Reuse normal-update protection for observer output. Remove protected properties
%% already stored in props, but leave physical columns under the migration's control.
migration_changes(Input, Resolved, Context) ->
    Removed = maps:map(fun(_Key, _Value) -> undefined end,
        maps:without(maps:keys(Resolved), Input)),
    Changed = maps:filter(fun(Key, Value) ->
        maps:find(Key, Input) =/= {ok, Value}
    end, Resolved),
    Editable = maps:filter(fun(Key, _Value) ->
        not m_rsc_update:is_protected(Key, true)
    end, maps:merge(Removed, Changed)),
    Columns = [atom_to_binary(Col, utf8) || Col <- z_db:column_names(rsc, Context)],
    ProtectedProps = maps:filter(fun(Key, _Value) ->
        m_rsc_update:is_protected(Key, true)
    end, maps:without(Columns, Input)),
    ClearProtected = maps:map(fun(_Key, _Value) -> undefined end, ProtectedProps),
    maps:merge(Editable, ClearProtected).

%% A negative privacy marks an unmigrated row. Its explicit value may still
%% live in legacy props; otherwise let rsc_get_raw initialize it once.
original(Id, #{<<"privacy">> := -1}, Context) ->
    case z_db:get_current_props(rsc, Id, Context) of
        {ok, Props} -> maps:get(<<"privacy">>, Props, undefined);
        _ -> undefined
    end;
original(_Id, Raw, _Context) ->
    maps:get(<<"privacy">>, Raw, undefined).

%% @doc A persistent, bounded sweep retries unresolved rows without starving later IDs.
-spec backfill(z:context()) -> {delay, pos_integer(), list()}.
backfill(Context) -> backfill(0, Context).

-spec backfill(non_neg_integer(), z:context()) -> {delay, pos_integer(), list()}.
backfill(After, Context) ->
    case migration_ready(Context) of
        false -> {delay, 10, [After]};
        true ->
            case z_db:transaction(fun(Ctx) -> batch(After, Ctx) end, Context) of
                {ok, []} -> {delay, 60, [0]};
                {ok, Ids} ->
                    lists:foreach(fun(Id) -> m_rsc_update:flush(Id, Context) end, Ids),
                    {delay, 1, [lists:last(Ids)]};
                Error ->
                    ?LOG_ERROR(#{text => <<"Resource defaults conversion failed; retrying batch">>,
                        in => zotonic_core, result => error, reason => Error, after_id => After}),
                    {delay, 60, [After]}
            end
    end.

batch(After, Context) ->
    ok = z_migration:batch_lock(Context),
    case migration_ready(Context) of
        false -> {ok, []};
        true ->
            Ids = [Id || {Id} <- z_db:q("select id from rsc where id > $1 and "
                "(privacy = -1 or props_json is null or content_group_id is null) "
                "order by id limit 100 for update", [After], Context)],
            lists:foreach(fun(Id) -> convert_safe(Id, Context) end, Ids),
            {ok, Ids}
    end.

%% @doc Revisit at most 100 IDs per transaction during a requested migration sweep.
%% Page over all IDs so even a database with mostly explicit privacy has bounded scans.
-spec rebuild(non_neg_integer(), z:context()) -> ok | {delay, pos_integer(), list()}.
rebuild(After, Context) ->
    case migration_ready(Context) of
        false -> {delay, 10, [After]};
        true ->
            case z_db:transaction(fun(Ctx) -> rebuild_batch(After, Ctx) end, Context) of
                {ok, [], _} -> ok;
                {ok, Ids, Changed} ->
                    lists:foreach(fun(Id) -> m_rsc_update:flush(Id, Context) end, Changed),
                    {delay, 1, [lists:last(Ids)]};
                Error ->
                    ?LOG_ERROR(#{text => <<"Resource defaults rebuild failed; retrying batch">>,
                        in => zotonic_core, result => error, reason => Error, after_id => After}),
                    {delay, 60, [After]}
            end
    end.

rebuild_batch(After, Context) ->
    ok = z_migration:batch_lock(Context),
    Rows = z_db:q("select id, privacy, (props_json is null or content_group_id is null) "
        "from rsc where id > $1 order by id limit 100 for update", [After], Context),
    Changed = [Id || {Id, Privacy, MissingJSON} <- Rows,
        Privacy =:= -1 orelse MissingJSON],
    % A failed conversion rolls back this bounded batch and retains its
    % cursor, so even previously resolved rows are retried without losing values.
    lists:foreach(fun(Id) ->
        case migration_ready(Context) of
            false -> error(defaults_paused);
            true -> convert(Id, Context)
        end
    end, Changed),
    {ok, [Id || {Id, _, _} <- Rows], Changed}.

%% Isolate a malformed row or observer error so the sweep can advance past it.
convert_safe(Id, Context) ->
    case migration_ready(Context) of
        false -> ok;
        true -> convert_unpaused(Id, Context)
    end.

convert_unpaused(Id, Context) ->
    z_db:q("savepoint rsc_defaults_row", Context),
    try convert(Id, Context) of
        {ok, 1} -> z_db:q("release savepoint rsc_defaults_row", Context)
    catch
        Class:Reason ->
            z_db:q("rollback to savepoint rsc_defaults_row", Context),
            z_db:q("release savepoint rsc_defaults_row", Context),
            ?LOG_ERROR(#{text => <<"Resource defaults conversion failed; row will be retried">>,
                in => zotonic_core, rsc_id => Id, result => Class, reason => Reason})
    end.

convert(Id, Context) ->
    %% Compare against stored values before rsc_get_raw supplies defaults or fixes.
    %% The batch already holds the row lock.
    {ok, Raw} = z_db:select(rsc, Id, Context),
    Changes = prepare(Id, #{}, Raw, true, Context),
    case maps:get(<<"privacy">>, Changes) of
        -1 ->
            ?LOG_WARNING(#{text => <<"Resource privacy unresolved; private query properties remain hidden">>,
                in => zotonic_core, rsc_id => Id});
        _ -> ok
    end,
    case migration_ready(Context) of
        false -> error(defaults_paused);
        true -> {ok, 1} = z_db:update(rsc, Id, Changes, Context)
    end.
