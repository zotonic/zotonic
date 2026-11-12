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

-module(z_traffic_guard_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_analytics/include/analytics.hrl").

settings() -> #{mode => enforce, period => 1000, request_limit => 2,
    request_burst => 2, max_clients => 2}.

mode_test() ->
    ?assertEqual(observe, z_traffic_guard:mode(undefined)),
    ?assertEqual(observe, z_traffic_guard:mode(<<"invalid">>)),
    ?assertEqual(off, z_traffic_guard:mode(<<"off">>)),
    ?assertEqual(enforce, z_traffic_guard:mode("enforce")).

request_rate_and_recovery_test() ->
    S = settings(),
    {A, E1} = z_traffic_guard:step(0,S,undefined),
    ?assertEqual(<<"allow">>, maps:get(guard_action,A)),
    {_, E2} = z_traffic_guard:step(0,S,E1),
    {B, E3} = z_traffic_guard:step(0,S,E2),
    ?assertEqual(<<"throttle">>, maps:get(guard_action,B)),
    ?assertEqual(<<"request_rate">>, maps:get(guard_reason,B)),
    ?assertEqual(1, maps:get(retry_after,B)),
    {_, E4} = z_traffic_guard:step(499,S,E3),
    {Recovered,_} = z_traffic_guard:step(500,S,E4),
    ?assertEqual(<<"allow">>, maps:get(guard_action,Recovered)).


observe_shadow_test() ->
    F = fun(Mode) ->
        lists:foldl(fun(_, {_,Entry}) ->
            z_traffic_guard:step(0,(settings())#{mode=>Mode},Entry)
        end, {undefined,undefined},lists:seq(1,3))
    end,
    {Observe,OState} = F(observe), {Enforce,EState} = F(enforce),
    ?assertEqual(OState,EState),
    ?assertEqual(<<"would_throttle">>,maps:get(guard_action,Observe)),
    ?assertEqual(<<"throttle">>,maps:get(guard_action,Enforce)).


scope_test() ->
    ?assert(mod_traffic_guard:eligible(<<"GET">>,controller_page,undefined)),
    ?assert(mod_traffic_guard:eligible(<<"HEAD">>,controller_agent_content,undefined)),
    ?assertNot(mod_traffic_guard:eligible(<<"GET">>,controller_lib,undefined)),
    ?assertNot(mod_traffic_guard:eligible(<<"POST">>,controller_page,true)),
    ?assertNot(mod_traffic_guard:eligible(<<"GET">>,controller_page,false)),
    ?assert(mod_traffic_guard:eligible(<<"GET">>,controller_custom,true)).

signals_test() ->
    Decision = (z_traffic_guard:decision(observe,request_rate,1))#{client => <<"private">>},
    C = z_context:set(traffic_guard_result,Decision,#context{}),
    Signals = mod_traffic_guard:signals(C),
    ?assertEqual(3,map_size(Signals)),
    ?assertNot(maps:is_key(client,Signals)),
    [Schema] = mod_traffic_guard:observe_analytics_events(#analytics_events{},[],C),
    ?assertMatch({ok,_},z_analytics:properties(Signals,Schema)),
    ?assertEqual(#{},mod_traffic_guard:signals(#context{})),
    lists:foreach(fun(Reason) ->
        R = z_traffic_guard:decision(enforce,Reason,0),
        ?assertEqual(<<"unknown">>,maps:get(crawl_behavior,R)),
        ?assertEqual(<<"allow">>,maps:get(guard_action,R))
    end,[state_capacity,unavailable]).

worker_test() ->
    C = #context{site = traffic_guard_unit_test},
    {ok,P} = z_traffic_guard:start_link([{context,C}]),
    try
        S = (settings())#{period => 60000, request_limit => 1},
        Results = [begin
            Ref = make_ref(), Parent = self(),
            spawn(fun() -> Parent ! {Ref,z_traffic_guard:check(client,S,C)} end),
            Ref
        end || _ <- lists:seq(1,12)],
        Replies = [receive {Ref,R} -> R after 3000 -> error(timeout) end || Ref <- Results],
        ?assertEqual(2,length([ok || #{guard_action:= <<"allow">>} <- Replies])),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(other,S,C))),
        ?assertEqual(<<"state_capacity">>,maps:get(guard_reason,z_traffic_guard:check(third,S,C))),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,S#{mode=>off},C))),
        z_traffic_guard:reset(C),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,S,C)))
    after gen_server:stop(P) end,
    ?assertEqual(<<"unavailable">>,maps:get(guard_reason,z_traffic_guard:check(client,settings(),C))).

supervision_test() ->
    ?assert(lists:member(supervisor, proplists:get_value(behaviour,mod_traffic_guard:module_info(attributes)))),
    C = #context{site = traffic_guard_supervision_test},
    {ok,Pid} = mod_traffic_guard:start_link([{context,C}]),
    unlink(Pid),
    try
        ?assertMatch([{z_traffic_guard,_,worker,_},{z_traffic_guard_rules,_,worker,_}],supervisor:which_children(Pid)),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,settings(),C)))
    after exit(Pid,shutdown) end.

request_signal_validation_test() ->
    Props = #{<<"type">>=><<"api">>, <<"method">>=><<"GET">>, <<"status">>=><<"2xx">>,
        <<"http_status">>=><<"200">>, <<"has_page_id">>=><<"false">>,
        <<"language">>=><<"en">>, <<"country">>=><<"unknown">>, <<"traffic">>=><<"agent">>,
        <<"crawl_behavior">>=><<"excessive">>, <<"guard_action">>=><<"would_throttle">>,
        <<"guard_reason">>=><<"request_rate">>},
    Schema = #{dimensions=>maps:map(fun(_,_) -> text end,Props)},
    ?assertEqual({ok,Props},z_analytics:properties(Props,Schema)),
    ?assertEqual({error,invalid_properties},z_analytics:properties(Props#{<<"ip">>=><<"private">>},Schema)),
    Large = maps:from_list([{integer_to_binary(I),<<"x">>} || I <- lists:seq(1,17)]),
    ?assertEqual({error,invalid_properties},z_analytics:properties(Large,#{dimensions=>maps:map(fun(_,_) -> text end,Large)})).

expired_work_test() ->
    State = #{clients=>#{},settings=>undefined},
    {reply,Result,State} = z_traffic_guard:handle_call(
        {check,client,settings(),erlang:monotonic_time(millisecond)-1},undefined,State),
    ?assertEqual(<<"unavailable">>,maps:get(guard_reason,Result)).


ip_parsing_test() ->
    ?assertEqual({ok,[{127,0,0,1},{192,0,2,1}]},m_traffic_guard:parse(
        <<", 127.0.0.1,192.0.2.1  ::ffff:127.0.0.1\t\r\n,">>)),
    ?assertEqual({ok,[]},m_traffic_guard:parse(<<", ,\t\n">>)),
    ?assertEqual({ok,[{127,0,0,1}]},m_traffic_guard:parse(<<"127.0.0.1\n::ffff:127.0.0.1\r\n">>)),
    ?assertEqual({ok,[{16#2001,16#db8,0,0,0,0,0,1}]},m_traffic_guard:parse(<<"2001:db8::1">>)),
    ?assertEqual({ok,[]},m_traffic_guard:parse(<<" \n\r\n">>)),
    [?assertEqual({error,invalid_ip_list},m_traffic_guard:parse(V)) || V <-
        [<<255>>,<<"localhost">>,<<"127.1">>,<<"127.0.0.1/8">>,<<"1.2.3.999">>,<<"fe80::1%en0">>,
         <<"1.2.3.4\nwrong">>,#{},undefined,binary:copy(<<"1.2.3.4\n">>,257),binary:copy(<<" ">>,16385)]].

ip_permissions_test() ->
    ?assertEqual({error,eacces},m_traffic_guard:lists(#context{})),
    ?assertEqual({error,eacces},m_traffic_guard:put(<<"allow">>,<<"127.0.0.1">>,<<"Test">>,<<"forever">>,#context{})),
    ?assertEqual({error,eacces},m_traffic_guard:put(<<"allow">>,<<"127.0.0.1">>,<<"Test">>,<<"forever">>,
        #context{user_id=1,acl_is_read_only=true})),
    ?assertEqual({error,invalid_entry},m_traffic_guard:put(<<"allow">>,<<"bad">>,<<"Test">>,<<"forever">>,#context{user_id=1})).

ip_admission_test() ->
    C=#context{site=traffic_guard_ip_test},
    {ok,P}=z_traffic_guard:start_link([{context,C}]),
    IP={192,0,2,1}, Other={192,0,2,2}, V6={16#2001,16#db8,0,0,0,0,0,1},
    S=(settings())#{ip_lists=>#{allow=>[IP],block=>[IP,Other,V6]}},
    try
        [ ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check(IP,S,C)))
            || _ <- lists:seq(1,10)],
        ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check({0,0,0,0,0,65535,49152,513},S,C))),
        [begin
            R=z_traffic_guard:check(X,S,C),
            ?assertEqual(<<"throttle">>,maps:get(guard_action,R)),
            ?assertEqual(<<"ip_block">>,maps:get(guard_reason,R)),
            ?assertEqual(<<"unknown">>,maps:get(crawl_behavior,R))
        end || X <- [Other,V6]],
        ?assertEqual(<<"would_throttle">>,maps:get(guard_action,z_traffic_guard:check(Other,S#{mode=>observe},C))),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(Other,S#{mode=>off},C))),
        ?assertEqual(0,map_size(maps:get(clients,sys:get_state(P))))
    after gen_server:stop(P) end.


admin_template_test() ->
    File="apps/zotonic_mod_traffic_guard/priv/templates/admin_traffic_guard.tpl",
    {ok,Source}=file:read_file(File),
    {ok,Tokens}=template_compiler_scanner:scan(File,Source),
    ?assertMatch({ok,_},template_compiler_parser:parse(Tokens)).


expiry_test() ->
    IP={192,0,2,1},
    ?assert(z_traffic_guard:matches(IP,#{IP=>0},100)),
    ?assert(z_traffic_guard:matches(IP,#{IP=>101},100)),
    ?assertNot(z_traffic_guard:matches(IP,#{IP=>100},100)),
    ?assertNot(z_traffic_guard:matches(IP,#{IP=>99},100)),
    [begin {ok,N}=m_traffic_guard:validity(K,0),?assertEqual(V,N) end || {K,V}<-
        [{<<"hour">>,3600},{<<"day">>,86400},{<<"week">>,604800},{<<"month">>,2592000},{<<"forever">>,0}]],
    ?assertMatch({ok,_},m_traffic_guard:validity(<<"2030-01-01T12:30">>,0)),
    [?assertEqual({error,invalid_expiry},m_traffic_guard:validity(V,2000000000)) || V<-
        [<<"2026-02-30T12:00">>,<<"2030-01-01T25:00">>,<<"2026-01-01T12:00">>,undefined]].

throttled_test() ->
    C=#context{site=traffic_guard_throttled_test},
    {ok,P}=z_traffic_guard:start_link([{context,C}]),
    IP={192,0,2,1}, S=(settings())#{request_burst=>1,request_limit=>1,period=>60000,mode=>observe},
    try
        z_traffic_guard:check(IP,S,C),
        z_traffic_guard:check(IP,S,C),
        {ok,[Row]}=z_traffic_guard:throttled(C),
        ?assertEqual(<<"192.0.2.1">>,maps:get(ip,Row)),
        ?assertEqual(<<"request_rate">>,maps:get(reason,Row)),
        ?assertEqual(<<"would_throttle">>,maps:get(action,Row)),
        ?assertEqual(1,maps:get(count,Row)),
        ?assertEqual({error,eacces},m_traffic_guard:throttled(#context{})),
        ?assertEqual({error,eacces},m_traffic_guard:block(<<"192.0.2.1">>,<<"day">>,#context{user_id=1,acl_is_read_only=true})),
        %% The administrator view uses the same indexed, expiry-aware rules.
        sys:replace_state(P,fun(State) ->
            State#{settings=>S#{ip_lists=>#{allow=>#{IP=>0},block=>#{}}}}
        end),
        ?assertEqual({ok,[]},z_traffic_guard:throttled(C)),
        sys:replace_state(P,fun(State) ->
            State#{settings=>S#{ip_lists=>#{allow=>#{IP=>1},block=>#{}}}}
        end),
        ?assertMatch({ok,[_]},z_traffic_guard:throttled(C)),
        z_traffic_guard:reset(C), ?assertEqual({ok,[]},z_traffic_guard:throttled(C))
    after gen_server:stop(P) end.


ip_index_test() ->
    IP={192,0,2,1}, Mapped={0,0,0,0,0,65535,49152,513},
    V6={16#2001,16#db8,0,0,0,0,0,1},
    Rows=[{IP,100},{Mapped,200},{V6,0},{V6,50}],
    Expected=#{IP=>200,V6=>0},
    ?assertEqual(Expected,z_traffic_guard:index(Rows)),
    ?assertEqual(Expected,z_traffic_guard:index(lists:reverse(Rows))),
    ?assert(z_traffic_guard:matches(IP,Expected,199)),
    ?assertNot(z_traffic_guard:matches(IP,Expected,200)),
    ?assert(z_traffic_guard:matches(V6,Expected,1000000)),
    ?assertNot(z_traffic_guard:matches({192,0,2,2},Expected,100)),
    ?assertEqual(#{IP=>0},z_traffic_guard:index([{IP,100},IP])).

expired_allow_block_test() ->
    C=#context{site=traffic_guard_expired_allow_test},
    {ok,P}=z_traffic_guard:start_link([{context,C}]),
    IP={192,0,2,1}, Now=erlang:system_time(second),
    S=(settings())#{ip_lists=>#{allow=>#{IP=>Now},block=>#{IP=>0}}},
    try
        ?assertEqual(<<"ip_block">>,maps:get(guard_reason,z_traffic_guard:check(IP,S,C))),
        Both=S#{ip_lists=>#{allow=>#{IP=>0},block=>#{IP=>0}}},
        ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check(IP,Both,C))),
        Expired=S#{ip_lists=>#{allow=>#{IP=>Now},block=>#{IP=>Now}}},
        ?assertEqual(<<"none">>,maps:get(guard_reason,z_traffic_guard:check(IP,Expired,C)))
    after gen_server:stop(P) end.


request_state_and_expiry_test() ->
    S=(settings())#{period=>1000,request_limit=>1,request_burst=>100},
    {Last,Entry}=lists:foldl(fun(_, {_,Previous}) ->
        z_traffic_guard:step(0,S,Previous)
    end,{undefined,undefined},lists:seq(1,101)),
    ?assertEqual(<<"request_rate">>,maps:get(guard_reason,Last)),
    ?assertEqual([expires,requests,time],lists:sort(maps:keys(Entry))),
    %% A low replenishment rate must not allow early eviction to forgive debt.
    ?assertEqual(100000,maps:get(expires,Entry)),
    {Recovered,Full}=z_traffic_guard:step(100000,S,Entry),
    ?assertEqual(<<"allow">>,maps:get(guard_action,Recovered)),
    ?assertEqual(99000,maps:get(requests,Full)).


analytics_page_scope_test() ->
    [?assert(mod_traffic_guard:eligible(Method,controller_custom,undefined,Page))
        || Method<-[<<"GET">>,<<"HEAD">>],Page<-[page,<<"page">>]],
    ?assertNot(mod_traffic_guard:eligible(<<"GET">>,controller_custom,false,page)),
    ?assertNot(mod_traffic_guard:eligible(<<"POST">>,controller_custom,true,page)),
    ?assertNot(mod_traffic_guard:eligible(<<"GET">>,controller_custom,undefined,api)).

ets_rules_test() ->
    C=#context{site=traffic_guard_ets_test}, Other=#context{site=traffic_guard_other_test},
    {ok,P}=z_traffic_guard_rules:start_link([{context,C}]),
    IP={192,0,2,1}, Now=erlang:system_time(second),
    try
        ?assertEqual(unavailable,z_traffic_guard_rules:lookup(IP,C)),
        ok=z_traffic_guard_rules:replace(#{allow=>#{IP=>0},block=>#{IP=>0}},C),
        ?assertEqual(ip_allow,z_traffic_guard_rules:lookup(IP,C)),
        ?assertEqual(unavailable,z_traffic_guard_rules:lookup(IP,Other)),
        %% There is no admission worker: rule hits must still decide immediately.
        ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check(IP,settings(),C))),
        ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check(
            {0,0,0,0,0,65535,49152,513},settings(),C))),
        ok=z_traffic_guard_rules:replace(#{allow=>#{IP=>Now},block=>#{IP=>0}},C),
        ok=z_traffic_guard_rules:prune(C),
        ?assertEqual(ip_block,z_traffic_guard_rules:lookup(IP,C)),
        ?assertEqual(<<"throttle">>,maps:get(guard_action,z_traffic_guard:check(IP,settings(),C))),
        ?assertEqual(<<"would_throttle">>,maps:get(guard_action,z_traffic_guard:check(IP,(settings())#{mode=>observe},C))),
        ok=z_traffic_guard_rules:replace(#{allow=>#{},block=>#{IP=>Now}},C),
        ?assertEqual(none,z_traffic_guard_rules:lookup(IP,C)),
        ok=z_traffic_guard_rules:prune(C),
        ?assertEqual([],ets:lookup(z_utils:name_for_site(traffic_guard_rules,z_context:site(C)),IP)),
        ok=z_traffic_guard_rules:replace(#{allow=>#{IP=>0},block=>#{}},C),
        ok=z_traffic_guard_rules:replace(#{allow=>#{},block=>#{}},C),
        ?assertEqual(none,z_traffic_guard_rules:lookup(IP,C)),
        ok=z_traffic_guard_rules:invalidate(C),
        ?assertEqual(unavailable,z_traffic_guard_rules:lookup(IP,C))
    after gen_server:stop(P) end.
