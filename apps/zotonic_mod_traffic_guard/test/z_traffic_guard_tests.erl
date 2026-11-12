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
    request_burst => 2, distinct_limit => 1, distinct_burst => 2, max_clients => 2}.

mode_test() ->
    ?assertEqual(observe, z_traffic_guard:mode(undefined)),
    ?assertEqual(observe, z_traffic_guard:mode(<<"invalid">>)),
    ?assertEqual(off, z_traffic_guard:mode(<<"off">>)),
    ?assertEqual(enforce, z_traffic_guard:mode("enforce")).

request_rate_and_recovery_test() ->
    S = settings(),
    {A, E1} = z_traffic_guard:step(<<"a">>,0,S,undefined),
    ?assertEqual(<<"allow">>, maps:get(guard_action,A)),
    {_, E2} = z_traffic_guard:step(<<"a">>,0,S,E1),
    {B, E3} = z_traffic_guard:step(<<"a">>,0,S,E2),
    ?assertEqual(<<"throttle">>, maps:get(guard_action,B)),
    ?assertEqual(<<"request_rate">>, maps:get(guard_reason,B)),
    ?assertEqual(1, maps:get(retry_after,B)),
    {_, E4} = z_traffic_guard:step(<<"a">>,499,S,E3),
    {Recovered,_} = z_traffic_guard:step(<<"a">>,500,S,E4),
    ?assertEqual(<<"allow">>, maps:get(guard_action,Recovered)).

distinct_targets_test() ->
    S = (settings())#{request_burst => 20},
    {_, E1} = z_traffic_guard:step(<<"a">>,0,S,undefined),
    {_, E2} = z_traffic_guard:step(<<"b">>,0,S,E1),
    {B, E3} = z_traffic_guard:step(<<"c">>,0,S,E2),
    ?assertEqual(<<"distinct_targets">>,maps:get(guard_reason,B)),
    {Repeated,E4} = z_traffic_guard:step(<<"a">>,0,S,E3),
    ?assertEqual(<<"allow">>,maps:get(guard_action,Repeated)),
    {Recovered,_} = z_traffic_guard:step(<<"c">>,1000,S,E4),
    ?assertEqual(<<"allow">>,maps:get(guard_action,Recovered)).

observe_shadow_test() ->
    F = fun(Mode) ->
        lists:foldl(fun(_, {_,Entry}) ->
            z_traffic_guard:step(<<"a">>,0,(settings())#{mode=>Mode},Entry)
        end, {undefined,undefined},lists:seq(1,3))
    end,
    {Observe,OState} = F(observe), {Enforce,EState} = F(enforce),
    ?assertEqual(OState,EState),
    ?assertEqual(<<"would_throttle">>,maps:get(guard_action,Observe)),
    ?assertEqual(<<"throttle">>,maps:get(guard_action,Enforce)).

bounded_targets_test() ->
    S = (settings())#{period=>100000,request_burst=>1000,distinct_burst=>1000},
    {_, E} = lists:foldl(fun(I,{_,Prev}) ->
        z_traffic_guard:step(integer_to_binary(I),0,S,Prev)
    end,{undefined,undefined},lists:seq(1,200)),
    ?assertEqual(64,map_size(maps:get(targets,E))),
    {_, Expired} = z_traffic_guard:step(<<"new">>,100000000,S,E),
    ?assertEqual(1,map_size(maps:get(targets,Expired))).

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
            spawn(fun() -> Parent ! {Ref,z_traffic_guard:check(client,<<"same">>,S,C)} end),
            Ref
        end || _ <- lists:seq(1,12)],
        Replies = [receive {Ref,R} -> R after 3000 -> error(timeout) end || Ref <- Results],
        ?assertEqual(2,length([ok || #{guard_action:= <<"allow">>} <- Replies])),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(other,<<"x">>,S,C))),
        ?assertEqual(<<"state_capacity">>,maps:get(guard_reason,z_traffic_guard:check(third,<<"x">>,S,C))),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,<<"x">>,S#{mode=>off},C))),
        z_traffic_guard:reset(C),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,<<"x">>,S,C)))
    after gen_server:stop(P) end,
    ?assertEqual(<<"unavailable">>,maps:get(guard_reason,z_traffic_guard:check(client,<<"x">>,settings(),C))).

supervision_test() ->
    ?assert(lists:member(supervisor, proplists:get_value(behaviour,mod_traffic_guard:module_info(attributes)))),
    C = #context{site = traffic_guard_supervision_test},
    {ok,Pid} = mod_traffic_guard:start_link([{context,C}]),
    unlink(Pid),
    try
        ?assertMatch([{z_traffic_guard,_,worker,_}],supervisor:which_children(Pid)),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(client,<<"x">>,settings(),C)))
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
        {check,client,<<"a">>,settings(),erlang:monotonic_time(millisecond)-1},undefined,State),
    ?assertEqual(<<"unavailable">>,maps:get(guard_reason,Result)).

expiry_boundary_test() ->
    S = (settings())#{request_burst=>100,distinct_burst=>10},
    {_,A} = z_traffic_guard:step(<<"a">>,0,S,undefined),
    {_,B} = z_traffic_guard:step(<<"b">>,500,S,A),
    {_,C} = z_traffic_guard:step(<<"b">>,1000,S,B),
    ?assertNot(maps:is_key(<<"a">>,maps:get(targets,C))),
    ?assert(maps:is_key(<<"b">>,maps:get(targets,C))),
    ?assertEqual(1500,maps:get(target_expiry,C)),
    %% Old worker entries from before the optimization are accepted as well.
    {_,D} = z_traffic_guard:step(<<"b">>,1500,S,maps:remove(target_expiry,C)),
    ?assertEqual(2500,maps:get(target_expiry,D)).


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
        [ ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check(IP,<<"a">>,S,C)))
            || _ <- lists:seq(1,10)],
        ?assertEqual(<<"ip_allow">>,maps:get(guard_reason,z_traffic_guard:check({0,0,0,0,0,65535,49152,513},<<"a">>,S,C))),
        [begin
            R=z_traffic_guard:check(X,<<"a">>,S,C),
            ?assertEqual(<<"throttle">>,maps:get(guard_action,R)),
            ?assertEqual(<<"ip_block">>,maps:get(guard_reason,R)),
            ?assertEqual(<<"unknown">>,maps:get(crawl_behavior,R))
        end || X <- [Other,V6]],
        ?assertEqual(<<"would_throttle">>,maps:get(guard_action,z_traffic_guard:check(Other,<<"a">>,S#{mode=>observe},C))),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_traffic_guard:check(Other,<<"a">>,S#{mode=>off},C))),
        ?assertEqual(0,map_size(maps:get(clients,sys:get_state(P))))
    after gen_server:stop(P) end.


admin_template_test() ->
    File="apps/zotonic_mod_traffic_guard/priv/templates/admin_traffic_guard.tpl",
    {ok,Source}=file:read_file(File),
    {ok,Tokens}=template_compiler_scanner:scan(File,Source),
    ?assertMatch({ok,_},template_compiler_parser:parse(Tokens)).


expiry_test() ->
    IP={192,0,2,1},
    ?assert(z_traffic_guard:matches(IP,[{IP,0}],100)),
    ?assert(z_traffic_guard:matches(IP,[{IP,101}],100)),
    ?assertNot(z_traffic_guard:matches(IP,[{IP,100}],100)),
    ?assertNot(z_traffic_guard:matches(IP,[{IP,99}],100)),
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
        z_traffic_guard:check(IP,<<"a">>,S,C),
        z_traffic_guard:check(IP,<<"a">>,S,C),
        {ok,[Row]}=z_traffic_guard:throttled(C),
        ?assertEqual(<<"192.0.2.1">>,maps:get(ip,Row)),
        ?assertEqual(<<"request_rate">>,maps:get(reason,Row)),
        ?assertEqual(<<"would_throttle">>,maps:get(action,Row)),
        ?assertEqual(1,maps:get(count,Row)),
        ?assertEqual({error,eacces},m_traffic_guard:throttled(#context{})),
        ?assertEqual({error,eacces},m_traffic_guard:block(<<"192.0.2.1">>,<<"day">>,#context{user_id=1,acl_is_read_only=true})),
        z_traffic_guard:reset(C), ?assertEqual({ok,[]},z_traffic_guard:throttled(C))
    after gen_server:stop(P) end.
