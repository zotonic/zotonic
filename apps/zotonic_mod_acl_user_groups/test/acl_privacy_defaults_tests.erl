%% @copyright 2026 Marc Worrell
%% @doc Verify stored defaults and legacy property conversion.
%% @end
%% Copyright 2026 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%     http://www.apache.org/licenses/LICENSE-2.0
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(acl_privacy_defaults_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

insert_defaults_test() ->
    C = z_context:new(acl_insert_defaults_fixture),
    meck:new(m_category, [passthrough, no_link]),
    meck:expect(m_category, is_a_prim, fun
        (Cat, person, Ctx) when Ctx =:= C -> Cat =:= 2;
        (Cat, Name, Ctx) -> meck:passthrough([Cat,Name,Ctx])
    end),
    meck:new(acl_user_groups_checks, [passthrough, no_link]),
    meck:expect(acl_user_groups_checks, default_content_group, fun
        (_, Ctx) when Ctx =:= C -> 42;
        (Cat, Ctx) -> meck:passthrough([Cat,Ctx])
    end),
    try
        Person = mod_acl_user_groups:observe_rsc_insert(#rsc_insert{props = #{}},
            #{<<"category_id">> => 2, <<"content_group_id">> => undefined}, C),
        ?assertMatch(#{<<"privacy">> := 30, <<"privacy_is_default">> := true,
            <<"content_group_id">> := 42}, Person),
        Public = mod_acl_user_groups:observe_rsc_insert(#rsc_insert{props = #{}},
            #{<<"category_id">> => 1}, C),
        ?assertMatch(#{<<"privacy">> := 0, <<"privacy_is_default">> := true}, Public),
        Explicit = mod_acl_user_groups:observe_rsc_insert(#rsc_insert{props =
            #{<<"privacy">> => 10, <<"content_group_id">> => 9}},
            #{<<"category_id">> => 2}, C),
        ?assertMatch(#{<<"privacy">> := 10, <<"privacy_is_default">> := false,
            <<"content_group_id">> := 9}, Explicit),
        %% Module-provided defaults survive the initial write even during startup.
        Stored = z_rsc_defaults:prepare(123, Person, Person, C),
        ?assertMatch(#{<<"privacy">> := 30, <<"privacy_is_default">> := true}, Stored)
    after
        meck:unload(m_category),
        meck:unload(acl_user_groups_checks)
    end.

migration_model_test() ->
    C = z_context:new(acl_migration_status_fixture),
    ?assertEqual({error,eacces}, m_acl_user_group:m_get([<<"privacy_migration">>], #{}, C)),
    meck:new(z_db, [passthrough, no_link]),
    try
        lists:foreach(fun(Pending) ->
            meck:expect(z_db, q1, fun
                ("select exists(select 1 from rsc where privacy_is_default is null "
                 "or privacy = -1 or props_json is null or content_group_id is null)", Ctx)
                        when Ctx#context.site =:= acl_migration_status_fixture -> Pending;
                (Sql,Ctx) -> meck:passthrough([Sql,Ctx])
            end),
            ?assertEqual({ok,{Pending,[]}}, m_acl_user_group:m_get(
                [<<"privacy_migration">>], #{}, z_acl:sudo(C)))
        end, [true,false])
    after
        meck:unload(z_db)
    end.
