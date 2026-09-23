%% @copyright 2026 Marc Worrell
%% @doc Verify stored defaults and legacy property conversion.
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
-module(z_rsc_defaults_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe_rsc_get/3]).

prepare_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    C = z_context:new(rsc_defaults_fixture),
    Key = {z_rsc_defaults, rsc_defaults_fixture},
    persistent_term:put(Key, false),
    ok = z_notifier:observe(rsc_get, {?MODULE, observe_rsc_get}, 100, C),
    meck:new(z_db, [passthrough, no_link]),
    meck:expect(z_db, get_current_props, fun
        (rsc, 12345, _) -> {ok, #{<<"privacy">> => 10}};
        (T, Id, Ctx) -> meck:passthrough([T, Id, Ctx])
    end),
    try
        Raw = #{<<"category_id">> => 1, <<"privacy">> => -1,
            <<"privacy_is_default">> => undefined},
        Explicit = z_rsc_defaults:prepare(12345, #{}, Raw, C),
        ?assertMatch(#{<<"privacy">> := 10, <<"privacy_is_default">> := false,
            <<"content_group_id">> := 9, <<"props_json">> := #{}}, Explicit),
        ?assertNot(maps:is_key(<<"computed">>, Explicit)),
        Default = Raw#{<<"privacy">> => 0, <<"privacy_is_default">> => true},
        ?assertMatch(#{<<"privacy">> := 30, <<"privacy_is_default">> := true},
            z_rsc_defaults:prepare(1, #{<<"category_id">> => 2}, Default, C)),
        ?assertMatch(#{<<"privacy">> := 0, <<"privacy_is_default">> := false},
            z_rsc_defaults:prepare(1, #{<<"privacy">> => 0}, Default, C)),
        ?assertMatch(#{<<"privacy">> := 30, <<"privacy_is_default">> := true},
            z_rsc_defaults:prepare(1, #{<<"privacy">> => undefined, <<"category_id">> => 2}, Default, C)),
        WithGroup = z_rsc_defaults:prepare(1, #{}, Default#{<<"content_group_id">> => 8}, C),
        ?assertNot(maps:is_key(<<"content_group_id">>, WithGroup)),
        ?assertMatch(#{<<"privacy">> := -1},
            z_rsc_defaults:prepare(1, #{<<"privacy">> => <<"bad">>}, Default, C)),
        persistent_term:put(Key, true),
        ?assertMatch(#{<<"privacy">> := -1}, z_rsc_defaults:prepare(1, #{}, Default, C))
    after
        meck:unload(z_db),
        z_notifier:detach(rsc_get, C),
        persistent_term:erase(Key)
    end.

observe_rsc_get(#rsc_get{}, Map, _) ->
    Privacy = case maps:get(<<"privacy">>, Map, undefined) of
        undefined -> case maps:get(<<"category_id">>, Map) of 2 -> 30; _ -> 0 end;
        P -> P
    end,
    Map#{<<"privacy">> => Privacy,
        <<"content_group_id">> => maps:get(<<"content_group_id">>, Map, 9),
        <<"computed">> => true}.

startup_resume_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    C = z_context:new(defaults_startup_fixture),
    Key = {z_rsc_defaults, defaults_startup_fixture},
    put(defaults_policy_fixture, undefined),
    put(defaults_rebuild_count, 0),
    meck:new(z_db, [passthrough, no_link]),
    meck:new(m_config, [passthrough, no_link]),
    meck:new(z_pivot_rsc, [passthrough, no_link]),
    meck:expect(z_db, has_connection, fun
        (Ctx) when Ctx =:= C -> true;
        (Ctx) -> meck:passthrough([Ctx])
    end),
    meck:expect(z_db, column_names, fun
        (rsc, Ctx) when Ctx =:= C -> [privacy_is_default];
        (T,Ctx) -> meck:passthrough([T,Ctx])
    end),
    meck:expect(z_db, transaction, fun
        (F,Ctx) when Ctx =:= C -> F(Ctx);
        (F,Ctx) -> meck:passthrough([F,Ctx])
    end),
    meck:expect(z_db, q, fun
        (Sql,Args,Ctx) when Ctx =:= C ->
            ?assertEqual(nomatch, binary:match(iolist_to_binary(Sql), <<"update rsc">>)),
            ?assert(is_list(Args)),
            0;
        (Sql,Args,Ctx) -> meck:passthrough([Sql,Args,Ctx])
    end),
    meck:expect(z_db, insert, fun
        (pivot_task_queue, Props, Ctx) when Ctx =:= C ->
            ?assertEqual(rebuild, maps:get(<<"function">>, Props)),
            ?assertEqual([0], maps:get(<<"args">>, Props)),
            put(defaults_rebuild_count, get(defaults_rebuild_count)+1),
            {ok, 1};
        (T,P,Ctx) -> meck:passthrough([T,P,Ctx])
    end),
    meck:expect(m_config, get_value, fun
        (z_rsc_defaults, policy, Ctx) when Ctx =:= C -> get(defaults_policy_fixture);
        (M,K,Ctx) -> meck:passthrough([M,K,Ctx])
    end),
    meck:expect(m_config, set_value, fun
        (z_rsc_defaults, policy, V, Ctx) when Ctx =:= C -> put(defaults_policy_fixture,V), ok;
        (M,K,V,Ctx) -> meck:passthrough([M,K,V,Ctx])
    end),
    meck:expect(z_pivot_rsc, insert_task, fun
        (z_rsc_defaults, backfill, <<>>, F, Ctx) when Ctx =:= C ->
            ?assertEqual({ok,{old_due,[123]}}, F(old_due,[123],new_due,Ctx)),
            ?assertEqual({ok,{new_due,[]}}, F(undefined,undefined,new_due,Ctx)),
            {ok, 2};
        (M,F,K,A,Ctx) -> meck:passthrough([M,F,K,A,Ctx])
    end),
    try
        ok = z_rsc_defaults:suspend(C),
        ?assertEqual(0, get(defaults_rebuild_count)),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(1, get(defaults_rebuild_count)),
        ok = z_rsc_defaults:suspend(C),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(1, get(defaults_rebuild_count)),
        z_notifier:observe(rsc_get, {?MODULE, observe_rsc_get}, 100, C),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(2, get(defaults_rebuild_count)),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(2, get(defaults_rebuild_count))
    after
        meck:unload(z_db),
        meck:unload(m_config),
        meck:unload(z_pivot_rsc),
        z_notifier:detach(rsc_get, C),
        persistent_term:erase(Key),
        erase(defaults_policy_fixture),
        erase(defaults_rebuild_count)
    end.
