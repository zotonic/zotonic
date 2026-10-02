%% @copyright 2026 Marc Worrell
%% @doc Collect migration status and serialize admin migration starts.
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
-module(z_migration).
-export([status/1, start/2, batch_lock/1]).
-include_lib("zotonic.hrl").

% Only migration batches/start requests use this lock; ordinary writes do not.
-define(MIGRATION_LOCK, 1920164965).

%% @doc Modules may add migration items or enrich the core resource migration item.
-spec status(z:context()) -> [map()].
status(Context) ->
    Items = z_notifier:foldl(#migration_status{}, [z_rsc_defaults:status(Context)], Context),
    case lists:any(fun(I) -> maps:get(is_running, I, false) end, Items) of
        true -> [I#{can_start => false} || I <- Items];
        false -> Items
    end.

%% @doc Protect the start boundary against currently executing migration batches.
-spec batch_lock(z:context()) -> ok.
batch_lock(Context) ->
    z_db:q("select pg_advisory_xact_lock_shared(hashtext(current_schema()), $1)", [?MIGRATION_LOCK], Context),
    ok.

%% @doc Start a known, needed migration only as admin and while all migrations are idle.
-spec start(binary(), z:context()) -> ok | {error, term()}.
start(Id, Context) ->
    case z_acl:is_admin(Context) of
        false -> {error, eacces};
        true ->
            z_db:transaction(fun(Ctx) ->
                case z_db:q1("select pg_try_advisory_xact_lock(hashtext(current_schema()), $1)",
                        [?MIGRATION_LOCK], Ctx) of
                    false -> {error, busy};
                    true -> start_idle(Id, status(Ctx), Ctx)
                end
            end, Context)
    end.

start_idle(Id, Items, Context) ->
    case lists:any(fun(I) -> maps:get(is_running, I, false) end, Items) of
        true -> {error, busy};
        false ->
            case [I || #{id := ItemId} = I <- Items, ItemId =:= Id,
                    maps:get(is_needed, I, false), maps:get(can_start, I, false)] of
                [_] when Id =:= <<"rsc_defaults">> -> z_rsc_defaults:invalidate(Context);
                [_] ->
                    case z_notifier:first(#migration_start{id = Id}, Context) of
                        undefined -> {error, unknown_migration};
                        Result -> Result
                    end;
                _ -> {error, not_needed}
            end
    end.
