%% @copyright 2026 Marc Worrell
%% @doc Exercise the resumable defaults migration against PostgreSQL.
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
-module(z_rsc_defaults_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1, observe_rsc_get/3, observe_migration_status/3]).

backfill_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    ok = z_db:transaction(fun run/1, C).

run(C0) ->
    %% Dedicated notifier/cache namespace; use the caller's transaction connection.
    C = C0#context{site = defaults_migration_fixture},
    Key = {z_rsc_defaults, defaults_migration_fixture},
    persistent_term:put(Key, false),
    z_notifier:observe(rsc_get, {?MODULE, observe_rsc_get}, 100, C),
    meck:new(z_db_table, [passthrough, no_link]),
    meck:expect(z_db_table, columns, fun
        (_, "rsc", #context{site = defaults_migration_fixture}) -> columns();
        (_, <<"rsc">>, #context{site = defaults_migration_fixture}) -> columns();
        (_, "pivot_task_queue", #context{site = defaults_migration_fixture}) -> task_columns();
        (_, <<"pivot_task_queue">>, #context{site = defaults_migration_fixture}) -> task_columns();
        (S,T,Ctx) -> meck:passthrough([S,T,Ctx])
    end),
    meck:new(z_depcache, [passthrough, no_link]),
    meck:expect(z_depcache, flush, fun
        (_, #context{site = defaults_migration_fixture}) -> ok;
        (K,Ctx) -> meck:passthrough([K,Ctx])
    end),
    meck:new(m_rsc, [passthrough, no_link]),
    meck:expect(m_rsc, is_a, fun
        (_, #context{site = defaults_migration_fixture}) -> [];
        (Id,Ctx) -> meck:passthrough([Id,Ctx])
    end),
    meck:new(z_pivot_rsc, [passthrough, no_link]),
    meck:expect(z_pivot_rsc, status, fun
        (#context{site = defaults_migration_fixture}) -> {ok, #{task_pid => undefined}};
        (Ctx) -> meck:passthrough([Ctx])
    end),
    meck:new(z_dispatcher, [passthrough, no_link]),
    meck:expect(z_dispatcher, url_for, fun
        (admin_status, #context{site = defaults_migration_fixture}) -> <<"/admin/status">>;
        (D,Ctx) -> meck:passthrough([D,Ctx])
    end),
    z_memo:delete(rsc_raw_sql),
    try
        z_db:q("create temporary table rsc (id integer primary key, category_id integer, "
            "content_group_id integer, privacy integer not null default -1, "
            "privacy_is_default boolean, props bytea, props_json jsonb)", C),
        z_db:q("create temporary table pivot_task_queue (id serial primary key, "
            "module varchar(80), function varchar(64), key varchar(100), props bytea, "
            "unique(module,function,key))", C),
        1 = z_db:q("insert into rsc(id,category_id,props,props_json) values(1,1,$1,$2)",
            [?DB_PROPS(#{<<"privacy">> => 50, <<"email">> => <<"legacy">>}),
             ?DB_PROPS_JSON(#{<<"privacy">> => 10, <<"email">> => <<"json">>})], C),
        4 = z_db:q("insert into rsc(id,category_id) values(2,2),(3,1),(4,99),(5,2)", C),
        %% Before conversion no row is eligible for public/member property queries.
        ?assertEqual(0, z_db:q1("select count(*) from rsc where privacy in (0,10)", C)),
        ?assertEqual({delay,1,[5]}, z_rsc_defaults:backfill(C)),
        ?assertEqual([{1,10,false,9},{2,30,true,9},{3,0,true,9},{4,-1,undefined,undefined},{5,30,true,9}],
            z_db:q("select id,privacy,privacy_is_default,content_group_id from rsc order by id", C)),
        [{undefined,JSON}] = z_db:q("select props,props_json from rsc where id=1", C),
        ?assertEqual(<<"json">>, maps:get(<<"email">>, JSON)),
        ?assertNot(maps:is_key(<<"privacy">>, JSON)),
        ?assertNot(maps:is_key(<<"computed">>, JSON)),
        %% Scheduling a policy change leaves existing privacy untouched.
        ok = z_rsc_defaults:invalidate(C),
        OldTask = z_db:q1("select id from pivot_task_queue", C),
        ?assertEqual([{1},{3}], z_db:q("select id from rsc where privacy in (0,10) order by id", C)),
        ok = z_rsc_defaults:invalidate(C),
        NewTask = z_db:q1("select id from pivot_task_queue", C),
        ?assert(NewTask > OldTask),
        ?assertEqual(0, z_db:q("delete from pivot_task_queue where id=$1", [OldTask], C)),
        ?assertEqual(NewTask, z_db:q1("select id from pivot_task_queue", C)),
        1 = z_db:q("update rsc set category_id=2 where id=3", C),
        1 = z_db:q("update rsc set category_id=1 where id=4", C),
        ?assertEqual({delay,1,[5]}, z_rsc_defaults:rebuild(0,C)),
        ?assertEqual([{1,10},{2,30},{3,30},{4,0},{5,30}],
            z_db:q("select id,privacy from rsc order by id", C)),
        ?assertEqual({delay,60,[0]}, z_rsc_defaults:backfill(5,C)),
        ?assertEqual(ok, z_rsc_defaults:rebuild(5,C)),
        200 = z_db:q("insert into rsc(id,category_id,privacy,privacy_is_default,props_json) "
            "select n,1,50,false,'{}'::jsonb from generate_series(6,205) n", C),
        ?assertEqual({delay,1,[105]}, z_rsc_defaults:rebuild(5,C)),
        ?assertEqual({delay,1,[205]}, z_rsc_defaults:rebuild(105,C)),
        ?assertEqual(ok, z_rsc_defaults:rebuild(205,C)),
        ?assertEqual(200, z_db:q1("select count(*) from rsc where id>5 and privacy=50", C)),
        z_db:q("delete from pivot_task_queue", C),
        ?assertNot(z_rsc_defaults:needed(C)),
        % A missing content group alone needs the same combined migration.
        1 = z_db:q("insert into rsc(id,category_id,privacy,privacy_is_default,props_json) "
            "values(206,1,10,false,'{}')", C),
        ?assert(z_rsc_defaults:needed(C)),
        z_notifier:observe(migration_status, {?MODULE, observe_migration_status}, 100, C),
        Busy = z_context:set(other_migration_fixture, true, z_acl:sudo(C)),
        ?assertEqual({error,busy}, z_migration:start(<<"rsc_defaults">>, Busy)),
        ?assert(lists:all(fun(I) -> not maps:get(can_start,I) end, z_migration:status(Busy))),
        ?assertEqual({error,eacces}, z_migration:start(<<"rsc_defaults">>, C)),
        ?assertEqual(ok, z_migration:start(<<"rsc_defaults">>, z_acl:sudo(C))),
        ?assertEqual({error,busy}, z_migration:start(<<"rsc_defaults">>, z_acl:sudo(C))),
        [#{is_running := true, can_start := false}] = z_migration:status(C),
        ?assertEqual({delay,1,[206]}, z_rsc_defaults:rebuild(205,C)),
        ?assertEqual([{10,9}], z_db:q("select privacy,content_group_id from rsc where id=206", C)),
        z_db:q("delete from pivot_task_queue", C),
        ?assertNot(z_rsc_defaults:needed(C)),
        ?assertEqual({error,not_needed}, z_migration:start(<<"rsc_defaults">>, z_acl:sudo(C))),
        ok
    after
        z_db:q("drop table if exists pg_temp.rsc", C),
        z_db:q("drop table if exists pg_temp.pivot_task_queue", C),
        meck:unload(z_db_table),
        meck:unload(z_depcache),
        meck:unload(m_rsc),
        meck:unload(z_pivot_rsc),
        meck:unload(z_dispatcher),
        z_notifier:detach(rsc_get,C),
        z_notifier:detach(migration_status,C),
        persistent_term:erase(Key),
        z_memo:delete(rsc_raw_sql)
    end.

observe_rsc_get(#rsc_get{}, #{<<"category_id">> := 99}, _) -> error(test_bad_row);
observe_rsc_get(#rsc_get{}, Map, _) ->
    P = case maps:get(<<"privacy">>,Map,undefined) of
        undefined -> case maps:get(<<"category_id">>,Map) of 2 -> 30; _ -> 0 end;
        V -> V
    end,
    CG = case maps:get(<<"content_group_id">>,Map,undefined) of undefined -> 9; V1 -> V1 end,
    Map#{<<"privacy">> => P, <<"content_group_id">> => CG, <<"computed">> => true}.

columns() -> [#column_def{name=N} || N <-
    [id,category_id,content_group_id,privacy,privacy_is_default,props,props_json]].

task_columns() -> [#column_def{name=N} || N <- [id,module,function,key,props]].

observe_migration_status(#migration_status{}, Items, Context) ->
    case z_context:get(other_migration_fixture, Context) of
        true -> [#{id => <<"other">>, is_running => true, can_start => false} | Items];
        _ -> Items
    end.
