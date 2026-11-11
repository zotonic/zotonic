%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Crawl guard admission, privacy and recovery regression tests.
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

-module(z_crawl_guard_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_analytics/include/analytics.hrl").

settings() -> #{mode => enforce, period => 1000, request_limit => 2,
    request_burst => 2, distinct_limit => 1, distinct_burst => 2, max_clients => 2}.

mode_test() ->
    ?assertEqual(observe, z_crawl_guard:mode(undefined)),
    ?assertEqual(observe, z_crawl_guard:mode(<<"invalid">>)),
    ?assertEqual(off, z_crawl_guard:mode(<<"off">>)),
    ?assertEqual(enforce, z_crawl_guard:mode("enforce")).

request_rate_and_recovery_test() ->
    S = settings(),
    {A, E1} = z_crawl_guard:step(<<"a">>,0,S,undefined),
    ?assertEqual(<<"allow">>, maps:get(guard_action,A)),
    {_, E2} = z_crawl_guard:step(<<"a">>,0,S,E1),
    {B, E3} = z_crawl_guard:step(<<"a">>,0,S,E2),
    ?assertEqual(<<"throttle">>, maps:get(guard_action,B)),
    ?assertEqual(<<"request_rate">>, maps:get(guard_reason,B)),
    ?assertEqual(1, maps:get(retry_after,B)),
    {_, E4} = z_crawl_guard:step(<<"a">>,499,S,E3),
    {Recovered,_} = z_crawl_guard:step(<<"a">>,500,S,E4),
    ?assertEqual(<<"allow">>, maps:get(guard_action,Recovered)).

distinct_targets_test() ->
    S = (settings())#{request_burst => 20},
    {_, E1} = z_crawl_guard:step(<<"a">>,0,S,undefined),
    {_, E2} = z_crawl_guard:step(<<"b">>,0,S,E1),
    {B, E3} = z_crawl_guard:step(<<"c">>,0,S,E2),
    ?assertEqual(<<"distinct_targets">>,maps:get(guard_reason,B)),
    {Repeated,E4} = z_crawl_guard:step(<<"a">>,0,S,E3),
    ?assertEqual(<<"allow">>,maps:get(guard_action,Repeated)),
    {Recovered,_} = z_crawl_guard:step(<<"c">>,1000,S,E4),
    ?assertEqual(<<"allow">>,maps:get(guard_action,Recovered)).

observe_shadow_test() ->
    F = fun(Mode) ->
        lists:foldl(fun(_, {_,Entry}) ->
            z_crawl_guard:step(<<"a">>,0,(settings())#{mode=>Mode},Entry)
        end, {undefined,undefined},lists:seq(1,3))
    end,
    {Observe,OState} = F(observe), {Enforce,EState} = F(enforce),
    ?assertEqual(OState,EState),
    ?assertEqual(<<"would_throttle">>,maps:get(guard_action,Observe)),
    ?assertEqual(<<"throttle">>,maps:get(guard_action,Enforce)).

bounded_targets_test() ->
    S = (settings())#{period=>100000,request_burst=>1000,distinct_burst=>1000},
    {_, E} = lists:foldl(fun(I,{_,Prev}) ->
        z_crawl_guard:step(integer_to_binary(I),0,S,Prev)
    end,{undefined,undefined},lists:seq(1,200)),
    ?assertEqual(64,map_size(maps:get(targets,E))),
    {_, Expired} = z_crawl_guard:step(<<"new">>,100000000,S,E),
    ?assertEqual(1,map_size(maps:get(targets,Expired))).

scope_test() ->
    ?assert(mod_crawl_guard:eligible(<<"GET">>,controller_page,undefined)),
    ?assert(mod_crawl_guard:eligible(<<"HEAD">>,controller_agent_content,undefined)),
    ?assertNot(mod_crawl_guard:eligible(<<"GET">>,controller_lib,undefined)),
    ?assertNot(mod_crawl_guard:eligible(<<"POST">>,controller_page,true)),
    ?assertNot(mod_crawl_guard:eligible(<<"GET">>,controller_page,false)),
    ?assert(mod_crawl_guard:eligible(<<"GET">>,controller_custom,true)).

signals_test() ->
    Decision = (z_crawl_guard:decision(observe,request_rate,1))#{client => <<"private">>},
    C = z_context:set(crawl_guard_result,Decision,#context{}),
    Signals = mod_crawl_guard:signals(C),
    ?assertEqual(3,map_size(Signals)),
    ?assertNot(maps:is_key(client,Signals)),
    [Schema] = mod_crawl_guard:observe_analytics_events(#analytics_events{},[],C),
    ?assertMatch({ok,_},z_analytics:properties(Signals,Schema)),
    ?assertEqual(#{},mod_crawl_guard:signals(#context{})),
    lists:foreach(fun(Reason) ->
        R = z_crawl_guard:decision(enforce,Reason,0),
        ?assertEqual(<<"unknown">>,maps:get(crawl_behavior,R)),
        ?assertEqual(<<"allow">>,maps:get(guard_action,R))
    end,[state_capacity,unavailable]).

worker_test() ->
    C = #context{site = crawl_guard_unit_test},
    {ok,P} = z_crawl_guard:start_link([{context,C}]),
    try
        S = (settings())#{period => 60000, request_limit => 1},
        Results = [begin
            Ref = make_ref(), Parent = self(),
            spawn(fun() -> Parent ! {Ref,z_crawl_guard:check(client,<<"same">>,S,C)} end),
            Ref
        end || _ <- lists:seq(1,12)],
        Replies = [receive {Ref,R} -> R after 3000 -> error(timeout) end || Ref <- Results],
        ?assertEqual(2,length([ok || #{guard_action:= <<"allow">>} <- Replies])),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_crawl_guard:check(other,<<"x">>,S,C))),
        ?assertEqual(<<"state_capacity">>,maps:get(guard_reason,z_crawl_guard:check(third,<<"x">>,S,C))),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_crawl_guard:check(client,<<"x">>,S#{mode=>off},C))),
        z_crawl_guard:reset(C),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_crawl_guard:check(client,<<"x">>,S,C)))
    after gen_server:stop(P) end,
    ?assertEqual(<<"unavailable">>,maps:get(guard_reason,z_crawl_guard:check(client,<<"x">>,settings(),C))).

supervision_test() ->
    ?assert(lists:member(supervisor, proplists:get_value(behaviour,mod_crawl_guard:module_info(attributes)))),
    C = #context{site = crawl_guard_supervision_test},
    {ok,Pid} = mod_crawl_guard:start_link([{context,C}]),
    unlink(Pid),
    try
        ?assertMatch([{z_crawl_guard,_,worker,_}],supervisor:which_children(Pid)),
        ?assertEqual(<<"allow">>,maps:get(guard_action,z_crawl_guard:check(client,<<"x">>,settings(),C)))
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
    {reply,Result,State} = z_crawl_guard:handle_call(
        {check,client,<<"a">>,settings(),erlang:monotonic_time(millisecond)-1},undefined,State),
    ?assertEqual(<<"unavailable">>,maps:get(guard_reason,Result)).

expiry_boundary_test() ->
    S = (settings())#{request_burst=>100,distinct_burst=>10},
    {_,A} = z_crawl_guard:step(<<"a">>,0,S,undefined),
    {_,B} = z_crawl_guard:step(<<"b">>,500,S,A),
    {_,C} = z_crawl_guard:step(<<"b">>,1000,S,B),
    ?assertNot(maps:is_key(<<"a">>,maps:get(targets,C))),
    ?assert(maps:is_key(<<"b">>,maps:get(targets,C))),
    ?assertEqual(1500,maps:get(target_expiry,C)),
    %% Old worker entries from before the optimization are accepted as well.
    {_,D} = z_crawl_guard:step(<<"b">>,1500,S,maps:remove(target_expiry,C)),
    ?assertEqual(2500,maps:get(target_expiry,D)).
