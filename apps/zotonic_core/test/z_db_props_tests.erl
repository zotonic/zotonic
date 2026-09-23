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
-module(z_db_props_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([run/1]).

json_precedence_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    ok = z_db:transaction(fun run/1, C).

%% Use a temporary table on the caller's transaction connection.
run(C) ->
    z_db:q("create temporary table defaults_props_test "
        "(id integer primary key, privacy integer, props bytea, props_json jsonb)", C),
    meck:new(z_db_table, [passthrough, no_link]),
    meck:expect(z_db_table, columns, fun
        (_, "defaults_props_test", _) -> columns();
        (_, <<"defaults_props_test">>, _) -> columns();
        (S, T, Ctx) -> meck:passthrough([S, T, Ctx])
    end),
    try
        Legacy = #{<<"email">> => <<"legacy">>, <<"only_legacy">> => true,
            <<"privacy">> => 0},
        JSON = #{<<"email">> => <<"json">>, <<"privacy">> => 10},
        1 = z_db:q("insert into defaults_props_test values (1,50,$1,$2)",
            [?DB_PROPS(Legacy), ?DB_PROPS_JSON(JSON)], C),
        lists:foreach(fun(Select) ->
            {ok, R} = z_db:qmap_props_row(Select, C),
            ?assertEqual(<<"json">>, maps:get(<<"email">>, R)),
            ?assertEqual(50, maps:get(<<"privacy">>, R)),
            ?assertEqual(true, maps:get(<<"only_legacy">>, R))
        end, ["select * from defaults_props_test", "select props_json, props, privacy from defaults_props_test"]),
        {ok, 1} = z_db:update(defaults_props_test, 1, #{<<"email">> => undefined}, C),
        [{undefined, Stored}] = z_db:q("select props, props_json from defaults_props_test", C),
        ?assertNot(maps:is_key(<<"email">>, Stored)),
        ?assertNot(maps:is_key(<<"privacy">>, Stored)),
        ?assertEqual(true, maps:get(<<"only_legacy">>, Stored)),
        {ok, 1} = z_db:update(defaults_props_test, 1, #{<<"other">> => 1}, C),
        {ok, Final} = z_db:select(defaults_props_test, 1, C),
        ?assertNot(maps:is_key(<<"email">>, Final)),
        ok
    after
        meck:unload(z_db_table),
        z_db:q("drop table pg_temp.defaults_props_test", C)
    end.

columns() -> [#column_def{name = N} || N <- [id, privacy, props, props_json]].
