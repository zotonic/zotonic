%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test versioned translation functions and SPARQL language projections.
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

-module(z_sparql_translation_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

helper_test() ->
    Context = context(),
    ok = mod_sparql:manage_schema(install, Context),
    ok = mod_sparql:manage_schema(install, Context),
    Trans = #trans{tr = [{en, <<"English">>}, {nl, <<"Nederlands">>}, {de, <<>>}]},
    lists:foreach(fun({Value, Lang, Default, Any, Expected}) ->
        ?assertEqual(Expected, z_db:q1(
            "SELECT z_sparql_translation_v1($1::jsonb, $2::text, $3::text, $4::boolean)",
            [Value, Lang, Default, Any], Context)),
        ?assertEqual(Expected, z_db:q1(
            "SELECT z_sparql_translation_v2($1::jsonb, $2::text, $3::text, $4::boolean)",
            [Value, Lang, Default, Any], Context))
    end, [
        {Trans, <<"nl">>, <<"en">>, false, tagged(<<"Nederlands">>, <<"nl">>)},
        {Trans, <<"NL">>, <<"en">>, false, tagged(<<"Nederlands">>, <<"nl">>)},
        {Trans, <<"fr">>, <<"nl">>, false, undefined},
        {Trans, <<"fr">>, <<"nl">>, true, tagged(<<"Nederlands">>, <<"nl">>)},
        {Trans, <<"fr">>, <<"es">>, true, tagged(<<"English">>, <<"en">>)},
        {Trans, <<"de">>, <<"nl">>, true, tagged(<<>>, <<"de">>)},
        {#trans{tr = [{fr, <<"France">>}, {de, <<"Deutschland">>}]},
            <<"nl">>, <<"es">>, true, tagged(<<"Deutschland">>, <<"de">>)},
        {#trans{}, <<"nl">>, <<"en">>, true, undefined},
        {null, <<"nl">>, <<"en">>, true, undefined},
        {42, <<"nl">>, <<"en">>, true, undefined},
        {#{<<"_type">> => <<"trans">>, <<"tr">> => []}, <<"nl">>, <<"en">>, true, undefined},
        {<<"Plain">>, <<"nl">>, <<"en">>, false, #{<<"value">> => <<"Plain">>}},
        {<<>>, <<"nl">>, <<"en">>, true, #{<<"value">> => <<>>}}
    ]),
    lists:foreach(fun(Type) ->
        Sql = ["SELECT z_sparql_translation_v1($1::", Type, ", 'nl', 'en', true)"],
        ?assertEqual(#{<<"value">> => <<"Plain">>}, z_db:q1(Sql, [<<"Plain">>], Context)),
        ?assertEqual(undefined, z_db:q1(Sql, [undefined], Context))
    end, ["text", "varchar"]),
    ?assertEqual(undefined, z_db:q1(
        "SELECT z_sparql_translation_v1(NULL::jsonb, 'nl', 'en', true)", Context)).

fallback_chain_test() ->
    Context = context(),
    ok = mod_sparql:manage_schema({upgrade, 2}, Context),
    lists:foreach(fun({Requested, Trans, Expected}) ->
        {ok, Code} = z_language:to_language_atom(Requested),
        Chain = [Code | z_language:fallback_language(Code)],
        ?assertEqual([atom_to_binary(L, utf8) || L <- Chain],
            z_db:q1("SELECT z_sparql_language_chain_v2($1)", [Requested], Context)),
        Result = z_db:q1("SELECT z_sparql_translation_v2($1::jsonb, $2, $3, true)",
            [Trans, Requested, atom_to_binary(z_language:default_language(Context), utf8)], Context),
        ?assertEqual(Expected, Result),
        ?assertEqual(z_trans:lookup_fallback(Trans, Chain, Context), maps:get(<<"value">>, Result))
    end, [
        {<<"nl-nl">>, #trans{tr = [{nl, <<"Dutch">>}, {en, <<"English">>}]}, tagged(<<"Dutch">>, <<"nl">>)},
        {<<"nl-nl">>, #trans{tr = [{'nl-nl', <<"Netherlands">>}, {nl, <<"Dutch">>}]},
            tagged(<<"Netherlands">>, <<"nl-nl">>)},
        {<<"en-gb">>, #trans{tr = [{en, <<"English">>}]}, tagged(<<"English">>, <<"en">>)},
        {<<"zh-tw">>, #trans{tr = [{'zh-hant', <<"Traditional">>}, {en, <<"English">>}]},
            tagged(<<"Traditional">>, <<"zh-hant">>)},
        {<<"zh-hant">>, #trans{tr = [{zh, <<"Simplified">>}, {en, <<"English">>}]},
            tagged(<<"Simplified">>, <<"zh">>)}
    ]),
    Trans = #trans{tr = [{nl, <<"Dutch">>}]},
    ?assertEqual(undefined, z_db:q1("SELECT z_sparql_translation_v1($1::jsonb, 'nl-nl', 'en', false)",
        [Trans], Context)),
    ?assertEqual(undefined, z_db:q1("SELECT z_sparql_translation_v2($1::jsonb, 'invalid-language', 'en', true)",
        [Trans], Context)),
    lists:foreach(fun(Type) ->
        Sql = ["SELECT z_sparql_translation_v2($1::", Type, ", 'nl-nl', 'en', true)"],
        ?assertEqual(#{<<"value">> => <<"Text">>}, z_db:q1(Sql, [<<"Text">>], Context)),
        ?assertEqual(undefined, z_db:q1(Sql, [undefined], Context))
    end, ["text", "varchar"]).

projection_test() ->
    Context = context(),
    ok = mod_sparql:manage_schema(install, Context),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => article,
        <<"name">> => <<"sparql_translation_test">>}, Context),
    try
        Trans = #trans{tr = [{en, <<"English">>}, {nl, <<"Nederlands">>}]},
        Id = z_db:q1("UPDATE rsc SET props_json = jsonb_set(props_json, '{title}', $1::jsonb) "
            "WHERE id = $2 RETURNING id", [Trans, Id], Context),
        Query = <<"SELECT ?s (zotonic:translation(?title, \"en\") AS ?en) "
            "(zotonic:translation(?title, ?lang) AS ?nl) "
            "(zotonic:translation(?title, \"fr\") AS ?missing) "
            "(zotonic:translationFallback(?title, \"fr\") AS ?fallback) "
            "(COALESCE(zotonic:translation(?title, \"fr\"), zotonic:translation(?title, \"nl\")) AS ?choice) "
            "(zotonic:translation(?name, \"nl\") AS ?plain) "
            "(zotonic:translation(\"Literal\", \"nl\") AS ?literal) "
            "(zotonic:translation(\"English\"@en, \"nl\") AS ?wrong_language) "
            "(zotonic:translation(zotonic:translation(?title, \"en\"), \"nl\") AS ?nested) "
            "(zotonic:translation(?absent, \"en\") AS ?absent_title) "
            "WHERE { ?s zotonic:id ?id . ?s zotonic:title ?title . ?s zotonic:name ?name . "
            "OPTIONAL { ?s zotonic:translation_absent ?absent } } ORDER BY ?nl">>,
        {ok, #{<<"results">> := #{<<"bindings">> := [Row]}}} = z_sparql_protocol:query(
            #{<<"query">> => Query, <<"args">> => #{<<"id">> => Id, <<"lang">> => <<"nl">>}}, Context),
        ?assertMatch(#{<<"value">> := <<"English">>, <<"xml:lang">> := <<"en">>}, maps:get(<<"en">>, Row)),
        ?assertMatch(#{<<"value">> := <<"Nederlands">>, <<"xml:lang">> := <<"nl">>}, maps:get(<<"nl">>, Row)),
        ?assertEqual(maps:get(<<"nl">>, Row), maps:get(<<"choice">>, Row)),
        Dynamic = <<"SELECT DISTINCT (zotonic:translationFallback(?title, ?lang) AS ?t) "
            "WHERE { ?s zotonic:id ?id . ?s zotonic:title ?title . "
            "VALUES ?lang { \"en\" \"nl-nl\" \"fr\" } } ORDER BY ?t LIMIT 2">>,
        {ok, #{<<"results">> := #{<<"bindings">> := [English, Dutch]}}} = z_sparql_protocol:query(
            #{<<"query">> => Dynamic, <<"args">> => #{<<"id">> => Id}}, Context),
        ?assertEqual(maps:get(<<"en">>, Row), maps:get(<<"t">>, English)),
        ?assertEqual(maps:get(<<"nl">>, Row), maps:get(<<"t">>, Dutch)),
        ModelQuery = <<"SELECT ?s (zotonic:translation(?v, \"nl\") AS ?text) "
            "WHERE { ?s zotonic:id ?id . VALUES ?v { \"English\"@en \"Nederlands\"@nl } }">>,
        {ok, #search_result{result = ModelRows}} = m_sparql:search(
            #{<<"q">> => [
                #{<<"term">> => <<"query">>, <<"value">> => ModelQuery},
                #{<<"term">> => <<"args">>, <<"value">> => #{<<"id">> => Id}}
            ]}, Context),
        ?assertEqual(lists:sort([{Id, undefined}, {Id, <<"Nederlands">>}]), lists:sort(ModelRows)),
        ?assertMatch({error, {invalid_function_arity, translation, 1}},
            z_sparql_protocol:query(#{<<"query">> =>
                <<"SELECT (zotonic:translation(?s) AS ?t) WHERE { ?s zotonic:id ?id }">>,
                <<"args">> => #{<<"id">> => Id}}, Context)),
        ?assert(maps:is_key(<<"fallback">>, Row)),
        ?assertMatch(#{<<"value">> := <<"sparql_translation_test">>}, maps:get(<<"plain">>, Row)),
        ?assertMatch(#{<<"value">> := <<"Literal">>}, maps:get(<<"literal">>, Row)),
        lists:foreach(fun(Key) -> ?assertNot(maps:is_key(Key, Row)) end,
            [<<"missing">>, <<"wrong_language">>, <<"nested">>, <<"absent_title">>])
    after
        m_rsc:delete(Id, Context)
    end.

tagged(Text, Lang) -> #{<<"value">> => Text, <<"language">> => Lang}.

context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).
