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
-export([observe_rsc_get_raw/3]).

prepare_test() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    C = z_context:new(rsc_defaults_fixture),
    Key = {z_rsc_defaults, rsc_defaults_fixture},
    persistent_term:put(Key, false),
    ok = z_notifier:observe(rsc_get_raw, {?MODULE, observe_rsc_get_raw}, 100, C),
    meck:new(z_db, [passthrough, no_link]),
    meck:expect(z_db, get_current_props, fun
        (rsc, 12345, _) -> {ok, #{<<"privacy">> => 10}};
        (T, Id, Ctx) -> meck:passthrough([T, Id, Ctx])
    end),
    try
        Raw = #{<<"category_id">> => 1, <<"privacy">> => -1},
        Explicit = z_rsc_defaults:prepare(12345, #{}, Raw, C),
        ?assertMatch(#{<<"privacy">> := 10,
            <<"content_group_id">> := 9, <<"props_json">> := #{}}, Explicit),
        ?assertNot(maps:is_key(<<"computed">>, Explicit)),
        Default = Raw#{<<"privacy">> => 0},
        ?assertMatch(#{<<"privacy">> := 0},
            z_rsc_defaults:prepare(1, #{<<"category_id">> => 2}, Default, C)),
        ?assertMatch(#{<<"privacy">> := 0},
            z_rsc_defaults:prepare(1, #{<<"privacy">> => 0}, Default, C)),
        ?assertMatch(#{<<"privacy">> := 30},
            z_rsc_defaults:prepare(1, #{<<"privacy">> => undefined, <<"category_id">> => 2}, Default, C)),
        WithGroup = z_rsc_defaults:prepare(1, #{}, Default#{<<"content_group_id">> => 8}, C),
        ?assertNot(maps:is_key(<<"content_group_id">>, WithGroup)),
        ?assertMatch(#{<<"privacy">> := -1},
            z_rsc_defaults:prepare(1, #{<<"privacy">> => <<"bad">>}, Default, C)),
        persistent_term:put(Key, true),
        ?assertMatch(#{<<"privacy">> := 0}, z_rsc_defaults:prepare(1, #{}, Default, C))
    after
        meck:unload(z_db),
        z_notifier:detach(rsc_get_raw, C),
        persistent_term:erase(Key)
    end.

observe_rsc_get_raw(#rsc_get_raw{}, Map, _) ->
    Privacy = case maps:get(<<"privacy">>, Map, undefined) of
        undefined -> case maps:get(<<"category_id">>, Map) of 2 -> 30; _ -> 0 end;
        P -> P
    end,
    Map#{<<"privacy">> => Privacy,
        <<"content_group_id">> => maps:get(<<"content_group_id">>, Map, 9),
        <<"computed">> => true}.

startup_resume_test() ->
    C = z_context:new(defaults_startup_fixture),
    Key = {z_rsc_defaults, defaults_startup_fixture},
    put(defaults_ready, false),
    put(defaults_starts, 0),
    meck:new(z_module_manager, [passthrough, no_link]),
    meck:new(z_db, [passthrough, no_link]),
    meck:new(z_pivot_rsc, [passthrough, no_link]),
    meck:expect(z_module_manager, all_running, fun
        (Ctx) when Ctx =:= C -> get(defaults_ready);
        (Ctx) -> meck:passthrough([Ctx])
    end),
    meck:expect(z_db, has_connection, fun
        (Ctx) when Ctx =:= C -> true;
        (Ctx) -> meck:passthrough([Ctx])
    end),
    meck:expect(z_db, column_names, fun
        (rsc, Ctx) when Ctx =:= C -> [privacy];
        (T,Ctx) -> meck:passthrough([T,Ctx])
    end),
    meck:expect(z_db, insert, fun
        (_, _, Ctx) when Ctx =:= C -> error(unexpected_automatic_rebuild);
        (T,P,Ctx) -> meck:passthrough([T,P,Ctx])
    end),
    meck:expect(z_pivot_rsc, insert_task, fun
        (z_rsc_defaults, backfill, <<>>, F, Ctx) when Ctx =:= C ->
            ?assertEqual({ok,{old_due,[123]}}, F(old_due,[123],new_due,Ctx)),
            ?assertEqual({ok,{new_due,[]}}, F(undefined,undefined,new_due,Ctx)),
            put(defaults_starts, get(defaults_starts) + 1),
            {ok, 2};
        (M,F,K,A,Ctx) -> meck:passthrough([M,F,K,A,Ctx])
    end),
    try
        ok = z_rsc_defaults:suspend(C),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(0, get(defaults_starts)),
        ?assertEqual({delay,10,[123]}, z_rsc_defaults:backfill(123,C)),
        ?assertEqual({delay,10,[123]}, z_rsc_defaults:rebuild(123,C)),
        put(defaults_ready, true),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(1, get(defaults_starts)),
        ok = z_rsc_defaults:suspend(C),
        ok = z_rsc_defaults:start(C),
        ?assertEqual(2, get(defaults_starts)),
        put(defaults_ready, false),
        ?assertEqual({delay,10,[123]}, z_rsc_defaults:backfill(123,C)),
        ?assertEqual({delay,10,[123]}, z_rsc_defaults:rebuild(123,C))
    after
        meck:unload(z_module_manager),
        meck:unload(z_db),
        meck:unload(z_pivot_rsc),
        persistent_term:erase(Key),
        erase(defaults_ready),
        erase(defaults_starts)
    end.
