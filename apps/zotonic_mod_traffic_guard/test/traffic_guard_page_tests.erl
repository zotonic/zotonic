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

-module(traffic_guard_page_tests).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

run(C) ->
    development = m_site:environment(C),
    true = z_module_manager:active(mod_traffic_guard,C),
    A = z_acl:sudo(C),
    ok = m_analytics:store_ensure_page_guard(C),
    Cs = m_analytics:store_collectors(C),
    [Guard] = [X || X <- Cs,m_analytics:is_page_guard(X)],
    [Page|_] = [X || X <- Cs,m_analytics:is_default_page(X)],
    %% Idempotence must not change any retained buckets or presentation metadata.
    ok = z_db:transaction(fun(Tx) ->
        z_db:q("select id from analytics_budget where id=1 for update",Tx),
        Before = z_db:q("select * from analytics_bucket order by collector_id,resolution,slot",Tx),
        Definitions = z_db:q("select * from analytics_collector order by id",Tx),
        ok = m_analytics:store_ensure_page_guard(Tx),
        ?assertEqual(Before,z_db:q("select * from analytics_bucket order by collector_id,resolution,slot",Tx)),
        ?assertEqual(Definitions,z_db:q("select * from analytics_collector order by id",Tx)),
        ok
    end,C),
    BudgetResult = z_db:transaction(fun(Tx) ->
        z_db:q("select id from analytics_budget where id=1 for update",Tx),
        ok = m_analytics:store_purge(maps:get(id,Guard),Tx),
        z_db:q("update analytics_budget set marker=marker & ~1 where id=1",Tx),
        fill_budget(1,Tx),
        ?assertEqual({error,budget_exceeded},m_analytics:store_ensure_page_guard(Tx)),
        ?assertEqual(0,z_db:q1("select marker & 1 from analytics_budget where id=1",Tx)),
        throw({rollback,budget_checked})
    end,C),
    ?assertEqual({rollback,budget_checked},BudgetResult),
    DeleteResult = z_db:transaction(fun(Tx) ->
        z_db:q("select id from analytics_budget where id=1 for update",Tx),
        ok=m_analytics:store_purge(maps:get(id,Guard),Tx),
        ok=m_analytics:store_ensure_page_guard(Tx),
        ?assertEqual([], [X || X <- m_analytics:store_collectors(Tx),m_analytics:is_page_guard(X)]),
        throw({rollback,deletion_checked})
    end,C),
    ?assertEqual({rollback,deletion_checked},DeleteResult),
    Report = maps:get(report,m_analytics:dashboard(maps:get(id,Page),<<"hour">>,A)),
    ?assertEqual(true,maps:get(guard_comparison,Report)),
    ?assertEqual(3,length(maps:get(chart,Report))),
    Overview = m_analytics:overview(<<"hour">>,A),
    ?assertNot(lists:any(fun(#{collector:=Col})->m_analytics:is_page_guard(Col) end,maps:get(panels,Overview))),
    Panel = #{collector=>Page,available=>true,report=>Report},
    Html = iolist_to_binary(z_template:render("_admin_analytics_panel.tpl",[{panel,Panel},{lead,true}],A)),
    ?assertNotEqual(nomatch,binary:match(Html,<<"Would throttle">>)),
    ?assertNotEqual(nomatch,binary:match(Html,<<"Throttled">>)),
    page_requests(Guard,C),
    #{companion_id=>maps:get(id,Guard),page_id=>maps:get(id,Page),chart_series=>3,
      rendered_bytes=>byte_size(Html),budget_and_history=>ok}.

fill_budget(N,C) ->
    #{reserved:=Used,budget:=Budget}=m_analytics:store_usage(C),
    G=min(100,(Budget-Used) div 3578),
    case G > 0 of
        false -> ok;
        true ->
            {ok,D}=z_analytics:definition(#{<<"name">>=><<"Budget test">>,<<"event">>=><<"page_request">>,
                <<"dimension">>=><<"language">>,<<"groups">>=>G,
                <<"filters">>=>#{<<"language">>=>[<<"guard-budget-",(integer_to_binary(N))/binary>>]}},z_analytics:registry(C)),
            case m_analytics:store_create(D,<<"paused">>,C) of
                {ok,_}->fill_budget(N+1,C);
                {error,budget_exceeded}->ok
            end
    end.

page_requests(Guard,C) ->
    Keys=[mode,period,request_limit,request_burst,distinct_burst],
    Old=[{K,m_config:get_value(mod_traffic_guard,K,C)}||K<-Keys],
    Id=maps:get(id,Guard), Before=counts(Id,C),
    URL=z_convert:to_list(z_context:abs_url(z_dispatcher:url_for(home,C),C)),
    try
        set([{period,<<"60">>},{request_limit,<<"1">>},{request_burst,<<"1">>},
            {distinct_burst,<<"64">>},{mode,<<"observe">>}],C),
        z_analytics_worker:reload(C), z_analytics_worker:status(C),
        ?assertEqual([200,200,200],[status(URL)||_<-lists:seq(1,3)]),
        set([{mode,<<"enforce">>}],C),
        ?assertEqual([200,429,429],[status(URL)||_<-lists:seq(1,3)]),
        wait(Id,Before,C,200)
    after
        [case V of undefined->m_config:delete(mod_traffic_guard,K,C); _->m_config:set_value(mod_traffic_guard,K,V,C) end||{K,V}<-Old]
    end.
set(Pairs,C) ->
    [m_config:set_value(mod_traffic_guard,K,V,C)||{K,V}<-Pairs],timer:sleep(100),z_traffic_guard:reset(C).
status(URL) ->
    {ok,{{_,Status,_},_,_}}=httpc:request(get,{URL,[{"user-agent","Mozilla/5.0"}]},
        [{ssl,[{verify,verify_none}]},{timeout,10000},{autoredirect,false}],[]),Status.
counts(Id,C) ->
    Rows=m_analytics:store_rows(Id,60,0,erlang:system_time(second)+60,C),
    lists:foldl(fun(#{counts:=Cs},Acc)->maps:fold(fun(K,V,A)->maps:update_with(K,fun(X)->X+V end,V,A) end,Acc,Cs) end,#{},Rows).
wait(_,_,_,0)->error(page_guard_counts_timeout);
wait(Id,Before,C,N)->
    Now=counts(Id,C),
    case lists:all(fun(K)->maps:get(K,Now,0)-maps:get(K,Before,0)=:=2 end,
        [<<"v:would_throttle">>,<<"v:throttle">>]) of
        true->ok;
        false->timer:sleep(100),wait(Id,Before,C,N-1)
    end.
