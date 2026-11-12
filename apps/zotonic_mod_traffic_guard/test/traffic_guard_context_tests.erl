%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Traffic guard admission, privacy and recovery regression tests.
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

-module(traffic_guard_context_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

body_deferral_test_() ->
    z_test_support:isolated({setup, fun setup/0, fun cleanup/1, fun body_deferral/0}).

setup() ->
    [meck:new(M,[passthrough,no_link]) || M <- [cowmachine_req,z_notifier,z_multipart_parse]],
    meck:expect(cowmachine_req,req_qs,fun(_) -> [{<<"query">>,<<"yes">>}] end),
    meck:expect(cowmachine_req,path_info,fun(_) -> #{id=><<"123">>} end),
    meck:expect(cowmachine_req,get_req_header,fun(<<"content-type">>,_) ->
        <<"application/x-www-form-urlencoded">>
    end),
    meck:expect(z_multipart_parse,config_optional_limit,fun(_,_) -> 1000 end),
    meck:expect(cowmachine_req,req_body,fun(_,C) -> {<<"body=yes">>,C} end),
    meck:expect(z_notifier,foldl,fun(#request_context{},C,_) ->
        %% Nested ensure_qs during authentication must not start body parsing.
        C = z_context:ensure_qs(C), C
    end),
    ok.
cleanup(_) -> [meck:unload(M) || M <- [cowmachine_req,z_notifier,z_multipart_parse]].

body_deferral() ->
    C = z_context:ensure_request_context(#context{}),
    ?assertEqual(0,meck:num_calls(cowmachine_req,req_body,'_')),
    ?assertEqual(2,meck:num_calls(z_notifier,foldl,'_')),
    ?assertEqual([{<<"id">>,<<"123">>},{<<"query">>,<<"yes">>}],z_context:get(q,C)),
    Parsed = z_context:ensure_qs(C),
    ?assertEqual(1,meck:num_calls(cowmachine_req,req_body,'_')),
    ?assertEqual(2,meck:num_calls(z_notifier,foldl,'_')),
    ?assertEqual([{<<"id">>,<<"123">>},{<<"body">>,<<"yes">>},{<<"query">>,<<"yes">>}],z_context:get(q,Parsed)),
    ?assertEqual(Parsed,z_context:ensure_qs(Parsed)),
    ?assertEqual(1,meck:num_calls(cowmachine_req,req_body,'_')).

configuration_cache_test_() ->
    z_test_support:isolated({setup, fun config_setup/0, fun config_cleanup/1, fun config_cache/0}).

config_setup() ->
    ets:new(guard_config_test,[named_table,public]),
    ets:insert(guard_config_test,[{mode,<<"observe">>},{request_burst,1}]),
    meck:new(m_config,[passthrough,no_link]),
    meck:new(m_traffic_guard,[passthrough,no_link]),
    meck:expect(m_traffic_guard,stored,fun(_) -> config_value(ip_lists,#{allow=>[],block=>[]}) end),
    meck:expect(m_config,get_value,fun(_,K,_) -> config_value(K,undefined) end),
    meck:expect(m_config,get_value,fun(_,K,D,_) -> config_value(K,D) end),
    meck:expect(m_config,set_value,fun(mod_traffic_guard,K,V,_) -> ets:insert(guard_config_test,{K,V}), ok end).
config_cleanup(_) -> meck:unload(m_traffic_guard),meck:unload(m_config),ets:delete(guard_config_test).
config_value(K,D) -> case ets:lookup(guard_config_test,K) of [{_,V}]->V; []->D end.
config_cache() ->
    C = #context{site=traffic_guard_cache_test},
    {ok,RulesPid} = z_traffic_guard_rules:start_link([{context,C}]),
    {ok,P} = z_traffic_guard:start_link([{context,C}]),
    try
        Old = z_traffic_guard:settings(C),
        ?assertNot(maps:is_key(ip_lists,Old)),
        ?assert(is_map(maps:get(settings,sys:get_state(P)))),
        [Old=z_traffic_guard:settings(C)||_<-lists:seq(1,100)],
        ?assertEqual(5,meck:num_calls(m_config,get_value,'_')),
        ets:insert(guard_config_test,{mode,<<"enforce">>}),
        z_traffic_guard:reset(C),
        %% A call serializes after reset even if no fresh HTTP settings lookup ran.
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,Old,C))),
        ?assertEqual(<<"throttle">>,maps:get(guard_action,z_traffic_guard:check(client,Old,C))),
        ?assertEqual(enforce,maps:get(mode,z_traffic_guard:settings(C))),
        ?assertEqual(10,meck:num_calls(m_config,get_value,'_')),
        ets:insert(guard_config_test,{ip_lists,#{allow=>[],block=>[{{192,0,2,1},0}]}}),
        ok=z_traffic_guard:reset(C),
        ?assertEqual(<<"ip_block">>,maps:get(guard_reason,z_traffic_guard:check({192,0,2,1},Old,C))),
        z_traffic_guard:check({192,0,2,2},Old,C),
        Clients=maps:get(clients,sys:get_state(P)),
        ets:insert(guard_config_test,{ip_lists,#{allow=>[{{192,0,2,1},0}],block=>[{{192,0,2,1},0}]}}),
        ok=z_traffic_guard:reload_rules(C),
        ?assertEqual(Clients,maps:get(clients,sys:get_state(P))),
        ?assertNot(maps:is_key(ip_lists,z_traffic_guard:settings(C))),
        ?assertEqual(#{allow=>#{{192,0,2,1}=>0},block=>#{{192,0,2,1}=>0}},
            maps:get(ip_lists,maps:get(settings,sys:get_state(P)))),
        ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check({192,0,2,1},Old,C)))
    after gen_server:stop(P),gen_server:stop(RulesPid) end.


global_config_test_() ->
    z_test_support:isolated(fun() ->
        application:set_env(zotonic,traffic_guard_allow,["192.0.2.1","::ffff:192.0.2.1"]),
        application:set_env(zotonic,traffic_guard_block,[<<"2001:db8::1">>]),
        Rows=m_traffic_guard:global(),
        ?assertEqual(3,length(Rows)),
        ?assert(lists:all(fun(#{source:=S,expires:=E})->S=:=<<"zotonic.config">> andalso E=:=0 end,Rows))
    end).
