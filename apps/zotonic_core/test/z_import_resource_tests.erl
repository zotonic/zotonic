%% @copyright 2026 Marc Worrell
%% @doc Tests for the shared resource import notification.
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

-module(z_import_resource_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([observe/2]).

import_notification_test_() -> {timeout, 60, fun import_notification/0}.

import_notification() ->
    Context = z_acl:logon(1, z_context:new(zotonic_site_testsandbox)),
    ok = z_module_manager:upgrade_await(Context),
    z_notifier:observe(import_resource, {?MODULE, observe}, 1, Context),
    try
        Result = z_db:transaction(fun check_imports/1, Context),
        ?assertEqual(checked, Result)
    after
        z_notifier:detach(import_resource, Context)
    end.

check_imports(Context) ->
    Uri = <<"https://example.org/import-resource-hook/root">>,
    ChildUri = <<"https://example.org/import-resource-hook/child">>,
    Data = #{<<"uri">> => Uri, <<"is_a">> => [<<"article">>],
        <<"resource">> => #{<<"title">> => <<"Notification test">>},
        <<"edges">> => #{<<"about">> => #{<<"objects">> => [
            #{<<"object_id">> => #{<<"uri">> => ChildUri, <<"is_a">> => [<<"article">>]}}
        ]}}},
    Notice = #import_resource{item = #{uri => Uri, props => #{}}, raw = Data},
    Default = fun(_) -> {skip, default} end,
    TestContext = z_context:set(import_resource_test, {self(), undefined}, Context),
    ?assertEqual({skip, default}, z_import_resource:import(Notice, Default, TestContext)),
    ?assertEqual(Notice, received()),
    SkipContext = z_context:set(import_resource_test, {self(), {skip, policy}}, Context),
    ?assertEqual({error, {skipped, policy}}, m_rsc_import:import(Data, SkipContext)),
    #import_resource{related = [#{uri := ChildUri}], raw = Data} = received(),
    ?assertEqual(undefined, m_rsc:uri_lookup(Uri, Context)),
    ?assertEqual(undefined, m_rsc:uri_lookup(ChildUri, Context)),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => article,
        <<"title">> => <<"Observer result">>, <<"uri">> => Uri,
        <<"is_authoritative">> => false}, Context),
    HandleContext = z_context:set(import_resource_test, {self(), {ok, Id}}, Context),
    ?assertMatch({ok, {Id, #{Uri := Id}}}, m_rsc_import:import(Data, HandleContext)),
    _ = received(),
    {ok, Status} = m_rsc_import:get_import_status(Id, Context),
    ?assertEqual(<<"ok">>, maps:get(<<"last_import_status">>, Status)),
    lists:foreach(fun(BadResult) ->
        BadContext = z_context:set(import_resource_test, {self(), BadResult}, Context),
        ?assertEqual({error, invalid_observer_result},
            z_import_resource:import(Notice, Default, BadContext)),
        _ = received()
    end, [ok, {ok, Id, #{invalid => Id}}]),
    AnonymousContext = z_context:set(import_resource_test, {self(), {ok, Id}}, z_acl:anondo(Context)),
    ?assertEqual({error, eacces}, z_import_resource:import(Notice, Default, AnonymousContext)),
    _ = received(),
    %% Same-URI delegation bypasses the hook; another resource still notifies.
    Delegate = fun(ImportContext) ->
        ?assertEqual({skip, default}, z_import_resource:import(Notice, Default, ImportContext)),
        Child = Notice#import_resource{item = #{uri => ChildUri, props => #{}}},
        ?assertEqual({skip, default}, z_import_resource:import(Child, Default, ImportContext)),
        {skip, delegated}
    end,
    ?assertEqual({skip, delegated}, z_import_resource:import(Notice, Delegate, TestContext)),
    ?assertEqual(Notice, received()),
    ?assertMatch(#import_resource{item = #{uri := ChildUri}}, received()),
    receive {import_notice, Extra} -> error({duplicate_notification, Extra}) after 0 -> ok end,
    %% Native fallback still persists a complete import through the new hook.
    NativeDefault = fun(ImportContext) ->
        {ok, {NativeId, Imported}} = m_rsc_import:import(Data, ImportContext),
        {ok, NativeId, Imported}
    end,
    ?assertMatch({ok, Id, _}, z_import_resource:import(Notice, NativeDefault, TestContext)),
    ?assertEqual(Notice, received()),
    receive {import_notice, Duplicate} -> error({duplicate_native_notification, Duplicate}) after 0 -> ok end,
    ?assertEqual(<<"Notification test">>, m_rsc:p(Id, title, Context)),
    {rollback, checked}.

observe(Notice, Context) ->
    case z_context:get(import_resource_test, Context) of
        {Pid, Result} -> Pid ! {import_notice, Notice}, Result;
        undefined -> undefined
    end.

received() ->
    receive {import_notice, Notice} -> Notice after 1000 -> error(missing_notification) end.
