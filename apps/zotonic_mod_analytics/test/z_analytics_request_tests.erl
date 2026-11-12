%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Page-view and API analytics permission regression tests.
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

-module(z_analytics_request_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

permissions_test_() ->
    z_test_support:isolated({setup, fun setup/0, fun cleanup/1, fun permissions/0}).

setup() ->
    Modules = [z_context,z_controller_helper,z_acl,m_config,m_rsc,
        z_analytics_worker,m_req,z_user_agent,z_module_manager,cowmachine_req],
    [meck:new(M,[passthrough,no_link]) || M <- Modules],
    meck:expect(z_analytics_worker,interested,fun(_,_) -> true end),
    meck:expect(z_context,get_resp_header,fun(_,_) -> <<"text/html">> end),
    meck:expect(z_context,set_req_metrics,fun(M,_) -> put(metrics,M), ok end),
    meck:expect(z_controller_helper,get_id,fun(C) -> z_context:get(test_id,C) end),
    meck:expect(z_controller_helper,get_configured_id,fun(C) -> z_context:get(test_id,C) end),
    meck:expect(z_controller_helper,is_authorized,fun(_,C) -> {true,C} end),
    meck:expect(z_acl,is_allowed,fun(use,mod_admin,C) -> z_context:get(test_admin,C) end),
    meck:expect(z_acl,rsc_editable,fun(Id,C) -> Id =:= z_context:get(editable_id,C) end),
    meck:expect(z_acl,anondo,fun(C) -> C end),
    meck:expect(m_rsc,is_visible,fun(_,C) -> not z_context:get(test_private,C,false) end),
    meck:expect(m_config,get_boolean,fun(mod_analytics,require_consent,C) ->
        z_context:get(test_consent_required,C,false) end),
    meck:expect(m_req,get,fun(_,_) -> <<>> end),
    meck:expect(z_user_agent,is_crawler,fun(_) -> false end),
    meck:expect(z_module_manager,active,fun(mod_geoip,_) -> false end),
    meck:expect(cowmachine_req,get_req_header,fun(_,_) -> undefined end),
    Modules.

cleanup(Modules) -> [meck:unload(M) || M <- Modules].

permissions() ->
    %% Editing this resource excludes views, independent of admin access.
    %% Admin access alone only excludes resource-less pages and API requests.
    lists:foreach(fun({Id,Editable,Admin,PageExpected,ApiExpected}) ->
        C = z_context:set([{test_id,Id},{editable_id,Editable},{test_admin,Admin}],#context{}),
        assert_mark(page,PageExpected,C),
        assert_mark(api,ApiExpected,C)
    end,[{10,10,false,false,true}, {10,10,true,false,false},
         {10,20,false,true,true}, {10,20,true,true,false},
         {undefined,20,false,true,true}, {undefined,20,true,false,false}]),
    C = z_context:set([{test_id,10},{editable_id,20},{test_admin,false}],#context{}),
    lists:foreach(fun(Type) ->
        assert_mark(Type,false,z_context:set(analytics_exclude,true,C)),
        Consent = z_context:set(test_consent_required,true,C),
        assert_mark(Type,false,Consent),
        assert_mark(Type,true,z_context:set(analytics_consent,true,Consent))
    end,[page,api]),
    assert_mark(page,false,z_context:set(test_private,true,C)),
    %% An explicit analytics resource overrides the controller's resource.
    assert_mark(page,false,z_context:set(analytics_page,20,C)).

assert_mark(Type,Expected,C) ->
    erase(metrics),
    Context = z_context:set(analytics,Type,C),
    ?assertEqual(Context,z_analytics_request:mark(Context)),
    Metrics = get(metrics),
    ?assertEqual(Expected,is_map(Metrics)),
    case {Expected,Type} of
        {true,page} ->
            ?assertMatch({<<"page_request">>,_},z_analytics_request:request_event(
                #{method=><<"GET">>,resp_status=>200,metrics=>Metrics})),
            ?assertMatch({<<"request">>,#{<<"type">>:=<<"page">>}},
                z_analytics_request:typed_event(#{method=><<"GET">>,resp_status=>200,metrics=>Metrics}));
        {true,api} -> ?assert(maps:is_key(analytics_api,Metrics));
        _ -> ok
    end.
