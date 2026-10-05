%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Test SPARQL endpoint queries, HTTP transport and access control.
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

-module(z_sparql_protocol_db_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

endpoint_query_test() ->
    Context = context(),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => article,
        <<"title">> => <<"SPARQL endpoint test">>, <<"is_published">> => false,
        <<"endpoint_number">> => 42}, Context),
    Args = #{<<"id">> => Id},
    Pattern = <<" WHERE { ?s zotonic:id ?id . VALUES ?word { \"hello\"@en \"hallo\"@nl } }">>,
    Query = <<"SELECT ?word ?s", Pattern/binary, " ORDER BY ?word LIMIT 1 OFFSET 1">>,
    try
        {ok, #{<<"head">> := #{<<"vars">> := [<<"word">>, <<"s">>]},
            <<"results">> := #{<<"bindings">> := [Binding]}}} = run(Query, Args, #{}, Context),
        ?assertMatch(#{<<"word">> := #{<<"value">> := <<"hello">>, <<"xml:lang">> := <<"en">>}}, Binding),
        ?assertEqual(#{<<"type">> => <<"uri">>, <<"value">> => m_rsc:uri(Id, Context)}, maps:get(<<"s">>, Binding)),
        {ok, #{<<"results">> := #{<<"bindings">> := [First]}}} = run(Query, Args,
            #{<<"page">> => 1, <<"pagelen">> => 1}, Context),
        ?assertMatch(#{<<"word">> := #{<<"value">> := <<"hallo">>}}, First),
        {ok, #{<<"results">> := #{<<"bindings">> := []}}} = run(
            <<"SELECT ?s", Pattern/binary, " LIMIT 0">>, Args, #{}, Context),
        {ok, #{<<"results">> := #{<<"bindings">> := [OnlyWord]}}} = run(
            <<"SELECT DISTINCT ?word", Pattern/binary, " LIMIT 1">>, Args, #{}, Context),
        ?assertEqual([<<"word">>], maps:keys(OnlyWord)),
        {ok, #{<<"results">> := #{<<"bindings">> := [Optional]}}} = run(
            <<"SELECT ?s ?missing WHERE { ?s zotonic:id ?id . OPTIONAL { ?s zotonic:endpoint_absent ?missing } }">>,
            Args, #{}, Context),
        ?assertEqual([<<"s">>], maps:keys(Optional)),
        {ok, #{<<"results">> := #{<<"bindings">> := [Number]}}} = run(
            <<"SELECT ?n WHERE { ?s zotonic:id ?id . ?s zotonic:endpoint_number ?n }">>, Args, #{}, Context),
        ?assertMatch(#{<<"n">> := #{<<"value">> := <<"42">>, <<"type">> := <<"literal">>}}, Number),
        {ok, #{<<"results">> := #{<<"bindings">> := [Count]}}} = run(
            <<"SELECT (COUNT(?s) AS ?count)", Pattern/binary>>, Args, #{}, Context),
        ?assertMatch(#{<<"count">> := #{<<"value">> := <<"2">>}}, Count),
        {ok, #{<<"head">> := #{<<"vars">> := StarVars}}} = run(
            <<"SELECT *", Pattern/binary, " LIMIT 1">>, Args, #{}, Context),
        ?assertEqual([<<"id">>, <<"s">>, <<"word">>], StarVars),
        {ok, #{<<"results">> := #{<<"bindings">> := [Expressions]}}} = run(
            <<"PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> SELECT (IF(true, \"ja\"@nl, \"yes\"@en) AS ?label) (true AS ?flag) "
              "(xsd:dateTime(\"2026-10-05T10:00:00Z\") AS ?date) WHERE { ?s zotonic:id ?id }">>,
            Args, #{}, Context),
        ?assertMatch(#{<<"label">> := #{<<"xml:lang">> := <<"nl">>},
            <<"flag">> := #{<<"value">> := <<"true">>},
            <<"date">> := #{<<"value">> := <<"2026-10-05T10:00:00Z">>}}, Expressions),
        {ok, #{<<"results">> := #{<<"bindings">> := [OptionalWord]}}} = run(
            <<"SELECT ?word WHERE { ?s zotonic:id ?id . OPTIONAL { VALUES ?word { \"hello\"@en } } }">>,
            Args, #{}, Context),
        ?assertMatch(#{<<"word">> := #{<<"xml:lang">> := <<"en">>}}, OptionalWord),
        {ok, #{<<"results">> := #{<<"bindings">> := Numbers}}} = run(
            <<"SELECT ?n WHERE { ?s zotonic:id ?id . VALUES ?n { 1 1.5e0 } } ORDER BY ?n">>,
            Args, #{}, Context),
        ?assertMatch([#{<<"n">> := #{<<"value">> := <<"1">>,
            <<"datatype">> := <<"http://www.w3.org/2001/XMLSchema#integer">>}}, _], Numbers),
        % The endpoint retains normal visibility restrictions.
        Anonymous = z_context:new(zotonic_site_testsandbox),
        {ok, #{<<"results">> := #{<<"bindings">> := []}}} = run(Query, Args, #{}, Anonymous),
        % Existing model results still contain resource ids, with no protocol envelope.
        ?assertMatch({ok, #search_result{result = [Id]}}, m_sparql:search(#{
            <<"query">> => <<"SELECT ?s WHERE { ?s zotonic:id ?id }">>, <<"args">> => Args}, Context)),
        ?assertEqual({error, {unsupported, limit}}, z_sparql:search(Query, Args, Context))
    after
        ok = m_rsc:delete(Id, Context)
    end.


translation_projection_test() ->
    Context = z_context:set_language(nl, context()),
    {ok, Id} = m_rsc:insert(#{<<"category_id">> => article,
        <<"title">> => <<"Translation projection">>}, Context),
    Query = <<"SELECT ?s ?title WHERE { ?s zotonic:id ?id . OPTIONAL { ?s zotonic:title ?title } }">>,
    try
        lists:foreach(fun({Title, Expected}) ->
            % Store multilingual fixtures directly: the sandbox's resource
            % updater removes translations for languages not enabled on the site.
            Id = z_db:q1("UPDATE rsc SET props_json = jsonb_set(props_json, '{title}', $1::text::jsonb) "
                "WHERE id = $2 RETURNING id", [z_json:encode(Title), Id], Context),
            {ok, #{<<"results">> := #{<<"bindings">> := [Row]}}} = run(
                Query, #{<<"id">> => Id}, #{}, Context),
            ?assertEqual(Expected, maps:get(<<"title">>, Row, undefined)),
            ?assert(maps:is_key(<<"s">>, Row))
        end, [
            {undefined, undefined},
            {<<"Plain title">>, #{<<"type">> => <<"literal">>, <<"value">> => <<"Plain title">>,
                <<"datatype">> => <<"http://www.w3.org/2001/XMLSchema#string">>}},
            {#trans{tr = [{en, <<"English">>}, {nl, <<"Nederlands">>}]}, tagged(<<"Nederlands">>, <<"nl">>)},
            {#trans{tr = [{en, <<"English">>}, {fr, <<"Francais">>}]}, tagged(<<"English">>, <<"en">>)},
            {#trans{tr = [{fr, <<"Francais">>}]}, tagged(<<"Francais">>, <<"fr">>)}
        ]),
        ?assertEqual(undefined, z_sparql_results:binding(#trans{tr = []}, undefined, undefined, undefined, Context)),
        % Secondary context preferences must win over English fallback.
        Preferred = z_context:set_language([de, fr, en], Context),
        ?assertEqual(tagged(<<"Francais">>, <<"fr">>), z_sparql_results:binding(
            #trans{tr = [{en, <<"English">>}, {fr, <<"Francais">>}]},
            undefined, undefined, undefined, Preferred))
    after
        ok = m_rsc:delete(Id, Context)
    end.

tagged(Text, Language) ->
    #{<<"type">> => <<"literal">>, <<"value">> => Text, <<"xml:lang">> => Language}.

http_transport_test_() -> {timeout, 30, fun http_transport/0}.

http_transport() ->
    Context = context(),
    ok = zotonic_listen_http:await(),
    Url = "https://localhost:" ++ integer_to_list(z_config:get(ssl_listen_port)) ++ "/sparql",
    Query = <<"SELECT ?s WHERE { ?s zotonic:id ?wanted } LIMIT 0">>,
    Args = #{<<"wanted">> => 1},
    Encoded = binary_to_list(cow_qs:qs([{<<"query">>, Query}, {<<"args">>, z_json:encode(Args)}])),
    Accept = [{"accept", "application/sparql-results+json"}],
    Options = [{ssl, [{verify, verify_none}]}, {timeout, 5000}, {autoredirect, false}],
    Requests = [
        {get, {Url ++ "?" ++ Encoded, Accept}},
        {post, {Url, Accept, "application/x-www-form-urlencoded", Encoded}},
        {post, {Url ++ "?args=" ++ binary_to_list(cow_qs:urlencode(z_json:encode(Args))),
                Accept, "application/sparql-query", Query}},
        {post, {Url, Accept, "application/json", z_json:encode(#{<<"query">> => Query, <<"args">> => Args})}}
    ],
    lists:foreach(fun({Method, Request}) ->
        {ok, {{_, Status, _}, Headers, Body}} = httpc:request(Method, Request, Options, [{body_format, binary}]),
        ?assertEqual({200, Method, Body}, {Status, Method, Body}),
        ?assertMatch("application/sparql-results+json" ++ _, proplists:get_value("content-type", Headers)),
        ?assertMatch(#{<<"head">> := #{<<"vars">> := [<<"s">>]}, <<"results">> := #{<<"bindings">> := []}}, z_json:decode(Body)),
        ?assertNotEqual(nomatch, string:find(proplists:get_value("cache-control", Headers), "no-store"))
    end, Requests),
    Invalid = [
        {Url, "application/json", <<"{">>, 400},
        {Url, "application/json", <<"[]">>, 400},
        {Url, "application/json", z_json:encode(#{<<"query">> => Query, <<"page">> => 0}), 400},
        {Url, "application/sparql-query", <<"SELECT WHERE {">>, 400},
        {Url, "application/sparql-query", <<"ASK { ?s ?p ?o }">>, 400},
        {Url, "text/plain", Query, 415},
        {Url, "application/x-www-form-urlencoded", Encoded ++ "&query=other", 400}
    ],
    lists:foreach(fun({Target, Type, Payload, Expected}) ->
        {ok, {{_, Status, _}, _, _}} = httpc:request(post, {Target, Accept, Type, Payload}, Options, []),
        ?assertEqual({Expected, Type, Payload}, {Status, Type, Payload})
    end, Invalid),
    lists:foreach(fun({Mime, Expected}) ->
        {ok, {{_, 200, _}, Headers, Body}} = httpc:request(get,
            {Url ++ "?" ++ Encoded, [{"accept", Mime}]}, Options, [{body_format, binary}]),
        ?assert(lists:prefix(Mime, proplists:get_value("content-type", Headers))),
        ?assertNotEqual(nomatch, binary:match(Body, Expected))
    end, [{"text/csv", <<"\"s\"\r\n">>}, {"text/tab-separated-values", <<"?s\n">>},
          {"application/sparql-results+xml", <<"<variable name=\"s\"/>">>}]),
    {ok, {{_, 406, _}, _, _}} = httpc:request(get,
        {Url ++ "?" ++ Encoded, [{"accept", "text/turtle"}]}, Options, []),
    ?assertEqual(<<"/sparql">>, z_dispatcher:url_for(sparql, Context)).

run(Query, Args, Paging, Context) ->
    z_sparql_protocol:query(Paging#{<<"query">> => Query, <<"args">> => Args}, Context).

context() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).
