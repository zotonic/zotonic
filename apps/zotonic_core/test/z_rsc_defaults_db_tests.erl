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
-export([run/1, observe_rsc_get_raw/3, observe_rsc_get/3, observe_migration_status/3]).

backfill_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    ok = z_db:transaction(fun run/1, C).

run(C0) ->
    %% Dedicated notifier/cache namespace; use the caller's transaction connection.
    C = C0#context{site = defaults_migration_fixture},
    Key = {z_rsc_defaults, defaults_migration_fixture},
    persistent_term:put(Key, false),
    z_notifier:observe(rsc_get_raw, {?MODULE, observe_rsc_get_raw}, 100, C),
    z_notifier:observe(rsc_get, {?MODULE, observe_rsc_get}, 100, C),
    meck:new(z_module_manager, [passthrough, no_link]),
    meck:expect(z_module_manager, all_running, fun
        (#context{site = defaults_migration_fixture}) -> true;
        (Ctx) -> meck:passthrough([Ctx])
    end),
    meck:expect(z_module_manager, active, fun
        (#context{site = defaults_migration_fixture}) -> [mod_fixture];
        (Ctx) -> meck:passthrough([Ctx])
    end),
    meck:expect(z_module_manager, get_modules_status, fun
        (#context{site = defaults_migration_fixture}) ->
            [{defaults_migration_fixture, running}, {mod_fixture, running}];
        (Ctx) -> meck:passthrough([Ctx])
    end),
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
            "creator_id integer default 42, version integer default 7, pivot_title text default 'original', "
            "props bytea, props_json jsonb)", C),
        z_db:q("create temporary table pivot_task_queue (id serial primary key, "
            "module varchar(80), function varchar(64), key varchar(100), props bytea, "
            "unique(module,function,key))", C),
        1 = z_db:q("insert into rsc(id,category_id,props,props_json) values(1,1,$1,$2)",
            [?DB_PROPS(#{<<"privacy">> => 50, <<"email">> => <<"legacy">>,
                <<"legacy_field">> => <<"old value">>,
                <<"computed_legacy">> => true, <<"page_url">> => <<"/legacy">>}),
             ?DB_PROPS_JSON(#{<<"privacy">> => 10, <<"email">> => <<"json">>,
                <<"computed_stored_json">> => true})], C),
        4 = z_db:q("insert into rsc(id,category_id) values(2,2),(3,1),(4,99),(5,2)", C),
        %% Before conversion no row is eligible for public/member property queries.
        ?assertEqual(0, z_db:q1("select count(*) from rsc where privacy in (0,10)", C)),
        ?assertEqual({delay,1,[5]}, z_rsc_defaults:backfill(C)),
        ?assertEqual([{1,10,9},{2,30,9},{3,0,9},{4,-1,undefined},{5,30,9}],
            z_db:q("select id,privacy,content_group_id from rsc order by id", C)),
        [{undefined,JSON}] = z_db:q("select props,props_json from rsc where id=1", C),
        ?assertEqual(<<"fixed-json">>, maps:get(<<"email">>, JSON)),
        ?assertNot(maps:is_key(<<"privacy">>, JSON)),
        ?assertEqual([], [Prop || Prop <- protected_props(), maps:is_key(Prop, JSON)]),
        ?assertNot(maps:is_key(<<"computed_legacy">>, JSON)),
        ?assertNot(maps:is_key(<<"computed_stored_json">>, JSON)),
        ?assertEqual([{42,7,<<"original">>}],
            z_db:q("select creator_id,version,pivot_title from rsc where id=1", C)),
        ?assertEqual(<<"old value">>, maps:get(<<"replacement_field">>, JSON)),
        ?assertNot(maps:is_key(<<"legacy_field">>, JSON)),
        %% A manual migration sweep leaves initialized privacy untouched.
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
        ?assertEqual([{1,10},{2,30},{3,0},{4,0},{5,30}],
            z_db:q("select id,privacy from rsc order by id", C)),
        ?assertEqual({delay,60,[0]}, z_rsc_defaults:backfill(5,C)),
        ?assertEqual(ok, z_rsc_defaults:rebuild(5,C)),
        200 = z_db:q("insert into rsc(id,category_id,privacy,props_json) "
            "select n,1,50,'{}'::jsonb from generate_series(6,205) n", C),
        ?assertEqual({delay,1,[105]}, z_rsc_defaults:rebuild(5,C)),
        ?assertEqual({delay,1,[205]}, z_rsc_defaults:rebuild(105,C)),
        ?assertEqual(ok, z_rsc_defaults:rebuild(205,C)),
        ?assertEqual(200, z_db:q1("select count(*) from rsc where id>5 and privacy=50", C)),
        z_db:q("delete from pivot_task_queue", C),
        ?assertNot(z_rsc_defaults:needed(C)),
        % A missing content group alone needs the same combined migration.
        1 = z_db:q("insert into rsc(id,category_id,privacy,props_json) "
            "values(206,1,10,'{}')", C),
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
        meck:unload(z_module_manager),
        meck:unload(z_depcache),
        meck:unload(m_rsc),
        meck:unload(z_pivot_rsc),
        meck:unload(z_dispatcher),
        z_notifier:detach(rsc_get_raw,C),
        z_notifier:detach(rsc_get,C),
        z_notifier:detach(migration_status,C),
        persistent_term:erase(Key),
        z_memo:delete(rsc_raw_sql)
    end.

observe_rsc_get_raw(#rsc_get_raw{}, #{<<"category_id">> := 99}, _) -> error(test_bad_row);
observe_rsc_get_raw(Event, #{<<"legacy_field">> := Legacy, <<"email">> := Email} = Map, Context) ->
    Fixed = (maps:remove(<<"legacy_field">>, Map))#{
        <<"replacement_field">> => Legacy,
        <<"email">> => <<"fixed-", Email/binary>>
    },
    observe_rsc_get_raw(Event, Fixed, Context);
observe_rsc_get_raw(#rsc_get_raw{is_props_only = true}, Map, _) ->
    Map;
observe_rsc_get_raw(#rsc_get_raw{is_props_only = false}, Map, _) ->
    P = case maps:get(<<"privacy">>,Map,undefined) of
        undefined -> case maps:get(<<"category_id">>,Map) of 2 -> 30; _ -> 0 end;
        V -> V
    end,
    CG = case maps:get(<<"content_group_id">>,Map,undefined) of undefined -> 9; V1 -> V1 end,
    Protected = maps:from_list([{Key, <<"observer value">>} || Key <- protected_props()]),
    (maps:merge(Map, Protected))#{<<"privacy">> => P, <<"content_group_id">> => CG}.

%% Computed read observers must never run during migration.
observe_rsc_get(#rsc_get{}, _Map, _Context) ->
    error(computed_observer_called_during_migration).

protected_props() ->
    [<<"id">>, <<"creator_id">>, <<"version">>, <<"privacy_is_default">>,
     <<"props">>, <<"props_json">>, <<"short_url">>, <<"page_url">>, <<"page_url_abs">>,
     <<"alternate_page_url">>, <<"alternate_page_url_abs">>, <<"email_raw">>,
     <<"medium">>, <<"pivot_title">>, <<"pivot_custom_field">>, <<"computed_test">>, <<"*internal">>].

columns() ->
    [#column_def{name = Name, type = Type} || {Name, Type} <- [
        {id, <<"integer">>}, {category_id, <<"integer">>},
        {content_group_id, <<"integer">>}, {privacy, <<"integer">>},
        {creator_id, <<"integer">>}, {version, <<"integer">>}, {pivot_title, <<"text">>},
        {props, <<"bytea">>}, {props_json, <<"jsonb">>}
    ]].

task_columns() ->
    [#column_def{name = Name, type = Type} || {Name, Type} <- [
        {id, <<"integer">>}, {module, <<"character varying">>},
        {function, <<"character varying">>}, {key, <<"character varying">>},
        {props, <<"bytea">>}
    ]].

observe_migration_status(#migration_status{}, Items, Context) ->
    case z_context:get(other_migration_fixture, Context) of
        true -> [#{id => <<"other">>, is_running => true, can_start => false} | Items];
        _ -> Items
    end.
