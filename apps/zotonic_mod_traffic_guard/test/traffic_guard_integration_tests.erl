%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Explicit development-site HTTP regression for traffic guard.
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

-module(traffic_guard_integration_tests).
-export([run/1]).
-include_lib("eunit/include/eunit.hrl").

%% Requires mod_agent_discovery and mod_analytics on an authorized local dev site.
run(C) ->
    development = m_site:environment(C),
    true = z_module_manager:active(mod_agent_discovery,C),
    true = z_module_manager:active(mod_analytics,C),
    Active = z_module_manager:active(mod_traffic_guard,C),
    Keys = [mode,period,request_limit,request_burst],
    Original = [{K,m_config:get_value(mod_traffic_guard,K,C)} || K <- Keys],
    try
        ok = z_module_manager:activate_await(mod_traffic_guard,C),
        configure([{period,60},{request_limit,1},{request_burst,2},
                   {mode,<<"observe">>}],C),
        z_analytics_worker:reload(C),
        z_analytics_worker:status(C),
        {ok,Id} = m_analytics:create(#{<<"name">>=><<"Traffic guard regression (temporary)">>,
            <<"event">>=><<"mod_traffic_guard:request">>, <<"dimension">>=><<"guard_action">>,
            <<"groups">>=>4, <<"filters">>=>#{<<"traffic">>=>[<<"agent">>]}},<<"active">>,z_acl:sudo(C)),
        try
            {ok,RequestId} = m_analytics:create(#{<<"name">>=><<"Traffic guard request signals (temporary)">>,
                <<"event">>=><<"request">>, <<"dimension">>=><<"guard_action">>, <<"groups">>=>4,
                <<"filters">>=>#{<<"traffic">>=>[<<"agent">>],<<"type">>=>[<<"agent_llms">>]}},
                <<"active">>,z_acl:sudo(C)),
            try test_http(Id,RequestId,C)
            after m_analytics:purge(RequestId,z_acl:sudo(C)) end
        after m_analytics:purge(Id,z_acl:sudo(C)) end
    after
        lists:foreach(fun
            ({K,undefined}) -> m_config:delete(mod_traffic_guard,K,C);
            ({K,V}) -> m_config:set_value(mod_traffic_guard,K,V,C)
        end,Original),
        case Active of false -> z_module_manager:deactivate(mod_traffic_guard,C); true -> ok end
    end.

configure(Pairs,C) ->
    lists:foreach(fun({K,V}) -> m_config:set_value(mod_traffic_guard,K,V,C) end,Pairs),
    %% Config notifications are asynchronous; let reset observers settle.
    timer:sleep(100),
    z_traffic_guard:reset(C).

test_http(Id,RequestId,C) ->
    URL = z_convert:to_list(z_context:abs_url(z_dispatcher:url_for(agent_llms,C),C)),
    z_analytics_worker:status(C),
    ?assertEqual([200,200,200],[status(URL) || _ <- lists:seq(1,3)]),
    configure([{mode,<<"enforce">>}],C),
    ?assertEqual([200,200],[status(URL) || _ <- lists:seq(1,2)]),
    {429,Headers,_} = request(URL),
    ?assertEqual("no-store",proplists:get_value("cache-control",Headers)),
    ?assert(list_to_integer(proplists:get_value("retry-after",Headers)) > 0),
    ?assertEqual("nosniff",proplists:get_value("x-content-type-options",Headers)),
    {ok,{{_,429,_},HeadHeaders,HeadBody}} = httpc:request(head,
        {URL,[{"user-agent","ChatGPT-User/1.0"}]},
        [{ssl,[{verify,verify_none}]},{timeout,10000}],[]),
    ?assertEqual([],HeadBody),
    ?assertEqual("no-store",proplists:get_value("cache-control",HeadHeaders)),
    assert_unread_body_rejected(URL),
    configure([{mode,<<"off">>}],C),
    ?assertEqual([200,200,200],[status(URL) || _ <- lists:seq(1,3)]),
    configure([{mode,<<"enforce">>},{period,1},{request_limit,2},{request_burst,1}],C),
    ?assertEqual(200,status(URL)),
    ?assertEqual(429,status(URL)),
    timer:sleep(600),
    ?assertEqual(200,status(URL)),
    %% Flush the actual worker, not synthetic events. Ten checked requests:
    %% six allowed, one observe-only rejection, and three enforced rejections.
    timer:sleep(200),
    whereis(z_utils:name_for_site(analytics_worker,z_context:site(C))) ! flush,
    wait_counts(Id, C, #{<<"v:allow">>=>6,<<"v:would_throttle">>=>1,<<"v:throttle">>=>3}, 150),
    %% Off-mode requests have no assessment and are grouped as unknown.
    wait_counts(RequestId, C, #{<<"v:allow">>=>6,<<"v:would_throttle">>=>1,<<"v:unknown">>=>3}, 150),
    ok.

wait_counts(_,_,_,0) -> error(analytics_timeout);
wait_counts(Id,C,Expected,N) ->
    Rows = m_analytics:store_rows(Id,60,0,erlang:system_time(second)+60,C),
    Counts = lists:foldl(fun(#{counts:=Cs},Acc) ->
        maps:fold(fun(K,V,A) -> maps:update_with(K,fun(X)->X+V end,V,A) end,Acc,Cs)
    end,#{},Rows),
    case Counts =:= Expected of
        true -> ok;
        false when N =:= 1 -> error({analytics_counts,Id,Expected,Counts});
        false -> timer:sleep(100), wait_counts(Id,C,Expected,N-1)
    end.

status(URL) -> {Status,_,_}=request(URL), Status.
request(URL) ->
    %% Local development certificates are commonly self-signed.
    {ok,{{_,Status,_},Headers,Body}} = httpc:request(get,
        {URL,[{"user-agent","ChatGPT-User/1.0"}]},
        [{ssl,[{verify,verify_none}]},{timeout,10000},{autoredirect,false}],[]),
    {Status,Headers,Body}.

%% A deliberately unfinished GET body must not make a throttled request wait.
assert_unread_body_rejected(URL) ->
    #{host:=Host,path:=Path}=Parts=uri_string:parse(URL),
    Port=maps:get(port,Parts,443),
    {ok,Socket}=ssl:connect(Host,Port,[{verify,verify_none},{active,false},{mode,binary}],2000),
    try
        ok=ssl:send(Socket,["GET ",Path," HTTP/1.1\r\nHost: ",Host,":",integer_to_list(Port),
            "\r\nUser-Agent: guard-audit-test\r\nContent-Type: application/x-www-form-urlencoded",
            "\r\nContent-Length: 1000000\r\nConnection: close\r\n\r\n"]),
        {ok,Response}=ssl:recv(Socket,0,2000),
        ?assertMatch({0,_},binary:match(Response,<<"HTTP/1.1 429">>))
    after ssl:close(Socket) end.
