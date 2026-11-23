-module(z_analytics_guard_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

guard_telemetry_test_() ->
    z_test_support:isolated({setup, fun setup/0, fun cleanup/1, fun telemetry/0}).

setup() ->
    Modules = [z_context,z_analytics_worker,m_analytics],
    [meck:new(M,[passthrough,no_link]) || M <- Modules],
    meck:expect(z_analytics_worker,interested,fun(_,_) -> get(interested) =:= true end),
    meck:expect(z_context,get_cookie,fun(_,_) -> undefined end),
    meck:expect(z_context,ensure_request_context,fun(Context) ->
        put(identity_calls,get(identity_calls)+1),
        case get(identity_fails) of true -> error(identity_failed); _ -> Context end
    end),
    meck:expect(m_analytics,record_event,fun(Name,Props,Req,Context) ->
        put(recorded,{Name,Props,Req,Context}),
        case get(record_fails) of true -> error(record_failed); _ -> ok end
    end),
    Modules.
cleanup(Modules) -> [meck:unload(M) || M <- Modules].

telemetry() ->
    Context = z_context:set(request_guard_result,
        #{source=>mod_traffic_guard,guard_action=><<"throttle">>},
        #context{controller_module=controller_page,cowreq=request()}),
    put(identity_calls,0),
    put(interested,false),
    ?assertEqual(ok,z_analytics_request:record_guard(rejected,Context)),
    ?assertEqual(0,get(identity_calls)),
    ?assertEqual(undefined,get(recorded)),
    put(interested,true),
    ?assertEqual(ok,z_analytics_request:record_guard(rejected,Context)),
    ?assertEqual(1,get(identity_calls)),
    ?assertMatch({<<"mod_traffic_guard:request">>,#{<<"type">>:=<<"page">>},#{},_},get(recorded)),
    ?assertEqual(ok,z_analytics_request:record_guard(handled,Context)),
    ?assertEqual(1,get(identity_calls)),
    put(identity_fails,true),
    ?assertEqual(ok,z_analytics_request:record_guard(rejected,Context)),
    {_,_,_,Excluded}=get(recorded),
    ?assertEqual(true,z_context:get(analytics_exclude,Excluded)),
    put(record_fails,true),
    ?assertEqual(ok,z_analytics_request:record_guard(handled,Context)),
    put(recorded,undefined),
    Other = z_context:set(request_guard_result,#{source=>some_other_guard},Context),
    ?assertEqual(ok,z_analytics_request:record_guard(handled,Other)),
    ?assertEqual(undefined,get(recorded)).

request() ->
    #{method=><<"GET">>,version=>'HTTP/1.1',scheme=><<"http">>,
      host=><<"localhost">>,port=>80,path=><<"/">>,qs=><<>>,headers=>#{},
      peer=>{{127,0,0,1},12345},sock=>{{127,0,0,1},80},cert=>undefined,
      ref=>?MODULE,pid=>self(),streamid=>1,has_body=>false,body_length=>0,
      bindings=>#{}}.
