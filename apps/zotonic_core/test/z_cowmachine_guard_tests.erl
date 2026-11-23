-module(z_cowmachine_guard_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

rejection_test_() ->
    z_test_support:isolated({setup, fun setup/0, fun cleanup/1, fun rejection/0}).

setup() ->
    Modules = [z_context,z_notifier,exometer,cowmachine_req,cowboy_req],
    [meck:new(M,[passthrough,no_link]) || M <- Modules],
    meck:expect(exometer,update,fun(_,_) -> ok end),
    meck:expect(z_context,init_cowdata,fun(Req,Env,Context) -> Context#context{cowreq=Req,cowenv=Env} end),
    meck:expect(z_context,set_csp_nonce,fun(Context) -> Context end),
    meck:expect(z_context,set_security_headers,fun(Context) -> Context end),
    meck:expect(cowmachine_req,init_env,fun(_,Env) -> Env end),
    meck:expect(cowboy_req,reply,fun(Status,Headers,Body,Req) -> Req#{resp_status=>Status,resp_headers=>Headers,resp_body=>Body} end),
    meck:expect(z_notifier,foldl,fun
        (#middleware{on=guard},Context,_) ->
            put(phases,[guard]),
            z_context:set(request_guard_response,{429,<<"Rejected">>},Context);
        (#middleware{on=rejected},Context,_) ->
            put(phases,get(phases)++[rejected]),
            case get(observer) of
                throw -> error(observer_failed);
                modify -> z_context:set(request_guard_response,{200,<<"Allowed">>},Context);
                absent -> Context
            end
    end),
    Modules.
cleanup(Modules) -> [meck:unload(M) || M <- Modules].

rejection() ->
    Env=#{cowmachine_controller=>controller_page,cowmachine_controller_options=>[],
        cowmachine_context=>#context{site=guard_test}},
    lists:foreach(fun(Observer) ->
        put(observer,Observer),
        Req=request(),
        ?assertEqual({stop,Req#{resp_status=>429,resp_headers=>#{},resp_body=><<"Rejected">>}},
            z_cowmachine_middleware:execute(Req,Env)),
        ?assertEqual([guard,rejected],get(phases))
    end,[absent,modify,throw]).

request() ->
    #{method=><<"GET">>,version=>'HTTP/1.1',scheme=><<"http">>,
      host=><<"localhost">>,port=>80,path=><<"/">>,qs=><<>>,headers=>#{},
      peer=>{{127,0,0,1},12345},sock=>{{127,0,0,1},80},cert=>undefined,
      ref=>?MODULE,pid=>self(),streamid=>1,has_body=>false,body_length=>0,
      bindings=>#{}}.
