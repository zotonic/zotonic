%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Zotonic
%% @doc Regression tests for version-checked admin saves and update notifications.
%% @end

%% Copyright 2026 Zotonic
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

-module(admin_edit_version_tests).
-author("Marc Worrell <marc@worrell.nl>").
-export([check/1]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

versioned_save_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    check(z_acl:logon(?ACL_ADMIN_USER_ID, z_context:new(zotonic_site_testsandbox))).

%% May also be run against a development site; only a temporary resource is changed.
check(Context) ->
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => <<"text">>,
        <<"is_published">> => false, <<"title">> => <<"Version conflict regression test">>,
        <<"language">> => [en, nl]}, Context),
    Topic = [<<"model">>, <<"rsc">>, <<"event">>, Id, <<"update">>],
    ok = z_mqtt:subscribe(Topic, Context),
    try
        V1 = m_rsc:p(Id, version, Context),
        Save = fun(Ctx) -> m_rsc:update_translation(Id, nl, #{<<"title">> => <<"Testtekst">>}, Ctx) end,
        ?assertEqual({error, stale}, admin_edit_version:save(Id, undefined, Save, Context)),
        {ok, V2} = admin_edit_version:save(Id, integer_to_binary(V1), Save, Context),
        ?assert(V2 > V1),
        ?assertEqual(V2, m_rsc:p(Id, version, Context)),
        ?assertEqual(V2, notification_version(Id)),
        ?assertEqual({error, stale}, admin_edit_version:save(Id, V1, Save, Context)),
        {ok, Id} = m_rsc:touch(Id, Context),
        ?assertEqual({error, stale}, admin_edit_version:save(Id, V2, Save, Context)),
        V3 = m_rsc:p(Id, version, Context),
        ?assert(V3 > V2),
        ?assertEqual(V3, notification_version(Id)),
        #trans{tr = Tr} = m_rsc:p(Id, title, Context),
        ?assertEqual(<<"Testtekst">>, proplists:get_value(nl, Tr)),
        ?assertEqual({error, test_failure}, admin_edit_version:save(Id, V3,
            fun(_Ctx) -> {error, test_failure} end, Context)),
        ok
    after
        z_mqtt:unsubscribe(Topic, Context),
        ok = m_rsc:delete(Id, Context)
    end.

notification_version(Id) ->
    receive
        {mqtt_msg, #{message := #{payload := #{id := Id, version := Version}}}} -> Version
    after 5000 -> error(missing_version_notification)
    end.
