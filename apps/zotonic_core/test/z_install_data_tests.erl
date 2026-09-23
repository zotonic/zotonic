%% @copyright 2026 Marc Worrell
%% @doc Verify bootstrap JSON properties and safe pending privacy defaults.
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
-module(z_install_data_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-export([run/1]).

bootstrap_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_db:transaction(fun run/1, Context).

%% @doc Exercise the installer on temporary tables without changing site resources.
run(Context) ->
    z_db:q("create temporary table rsc (id integer primary key, "
        "is_protected boolean, visible_for integer, category_id integer, name text, uri text, "
        "props bytea, props_json jsonb, language text[], publication_start timestamp, "
        "privacy integer not null default -1, privacy_is_default boolean, "
        "creator_id integer, modifier_id integer, is_published boolean)", Context),
    z_db:q("create temporary table hierarchy (name text, id integer, parent_id integer, "
        "nr integer, lvl integer, lft integer, rght integer)", Context),
    z_db:q("create temporary table identity (rsc_id integer, type text, key text, "
        "is_unique boolean, propb bytea)", Context),
    z_db:q("create temporary table predicate_category "
        "(predicate_id integer, is_subject boolean, category_id integer)", Context),
    z_db:q("create temporary sequence rsc_id_seq", Context),
    meck:new(m_identity, [passthrough, no_link]),
    meck:expect(m_identity, hash, fun(<<>>) -> bootstrap_test_hash end),
    try
        ok = z_install_data:install(z_context:site(Context), Context),
        ?assert(z_db:q1("select count(*) from pg_temp.rsc", Context) > 20),
        ?assertEqual(0, z_db:q1("select count(*) from pg_temp.rsc where "
            "props is not null or props_json is null or jsonb_typeof(props_json) <> 'object' "
            "or privacy <> -1 or privacy_is_default is distinct from true", Context)),
        [Admin] = z_db:q("select props_json from pg_temp.rsc where id=1", Context),
        ?assertEqual({#{<<"title">> => <<"Site Administrator">>}}, Admin),
        [{Category}] = z_db:q("select props_json from pg_temp.rsc where id=116", Context),
        ?assertMatch(#trans{}, maps:get(<<"title">>, Category)),
        [{Predicate}] = z_db:q("select props_json from pg_temp.rsc where id=308", Context),
        ?assertEqual(true, maps:get(<<"is_connect_checkbox">>, Predicate)),
        ?assertEqual(false, maps:get(<<"reversed">>, Predicate)),
        ?assertMatch(#trans{}, maps:get(<<"title">>, Predicate)),
        ok
    after
        meck:unload(m_identity),
        z_db:q("drop table pg_temp.predicate_category, pg_temp.identity, "
            "pg_temp.hierarchy, pg_temp.rsc", Context),
        z_db:q("drop sequence pg_temp.rsc_id_seq", Context)
    end.
