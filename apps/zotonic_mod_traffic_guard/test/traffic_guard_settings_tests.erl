%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Traffic guard settings validation and authorization tests.
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

-module(traffic_guard_settings_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

settings_test_() ->
    z_test_support:isolated(fun settings/0).

settings() ->
    Anonymous = #context{},
    Admin = #context{user_id = 1},
    ReadOnly = Admin#context{acl_is_read_only = true},
    Modules = [z_db, m_config, z_depcache, z_traffic_guard],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(z_db, transaction, fun(F, C) -> F(C) end),
        meck:expect(m_config, set_value, fun(mod_traffic_guard, _, _, _) -> ok end),
        meck:expect(z_depcache, flush, fun(config, _) -> ok end),
        meck:expect(z_traffic_guard, reset, fun(_) -> ok end),
        meck:expect(z_traffic_guard, settings, fun(_) ->
            #{mode => observe, period => 60000, request_limit => 120,
              request_burst => 30, max_clients => 5000}
        end),
        Valid = #{<<"mode">> => <<"observe">>, <<"period">> => <<"60">>,
            <<"request_limit">> => <<"120">>, <<"request_burst">> => <<"30">>,
            <<"max_clients">> => <<"5000">>},
        ?assertEqual({error, eacces}, m_traffic_guard:settings(Anonymous)),
        {ok, Settings} = m_traffic_guard:settings(Admin),
        ?assertEqual(60, maps:get(period, Settings)),
        ?assertEqual({error, eacces}, m_traffic_guard:save_settings(Valid, Anonymous)),
        ?assertEqual({error, eacces}, m_traffic_guard:save_settings(Valid, ReadOnly)),
        lists:foreach(fun(Invalid) ->
            ?assertEqual({error, invalid_settings}, m_traffic_guard:save_settings(Invalid, Admin))
        end, [#{}, undefined, maps:remove(<<"period">>, Valid),
            Valid#{<<"mode">> := <<"invalid">>},
            Valid#{<<"period">> := <<"0">>}, Valid#{<<"period">> := <<"3601">>},
            Valid#{<<"period">> := <<"1.5">>}, Valid#{<<"period">> := true},
            Valid#{<<"request_limit">> := <<"100001">>},
            Valid#{<<"request_burst">> := <<"1001">>},
            Valid#{<<"max_clients">> := <<"50001">>}]),
        ?assertEqual(0, meck:num_calls(m_config, set_value, '_')),
        lists:foreach(fun(Mode) ->
            ?assertEqual(ok, m_traffic_guard:save_settings(Valid#{<<"mode">> := Mode}, Admin))
        end, [<<"off">>, <<"observe">>, <<"enforce">>]),
        ?assertEqual(15, meck:num_calls(m_config, set_value, '_')),
        ?assert(meck:called(m_config, set_value, [mod_traffic_guard, period, 60, Admin])),
        ?assertEqual(3, meck:num_calls(z_traffic_guard, reset, '_')),
        meck:expect(z_db, transaction, fun(_, _) -> {rollback, failed} end),
        ?assertEqual({error, failed}, m_traffic_guard:save_settings(Valid, Admin)),
        ?assertEqual(3, meck:num_calls(z_traffic_guard, reset, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
