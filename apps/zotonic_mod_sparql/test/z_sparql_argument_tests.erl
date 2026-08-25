-module(z_sparql_argument_tests).
-moduledoc("SPARQL pre-bound argument tests.").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

-export([
    observe_rdf_ns/2,
    observe_sparql_mapping/2
]).

argument_normalization_test() ->
    Context = test_context(),
    {ok, Query} = z_sparql:parse(<<
        "SELECT ?r WHERE { ?r <https://example.test/name> ?name }"
    >>),
    DateTime = {{2008, 12, 10}, {11, 12, 13}},
    {ok, #{ arguments := Arguments }} = z_sparql_plan:to_query_plan(Query, #{
        enabled => true,
        status => draft,
        <<"count">> => 42,
        <<"ratio">> => 1.5,
        <<"date">> => {2008, 12, 10},
        <<"datetime">> => DateTime,
        <<"json_date">> => #{
            <<"type">> => <<"date">>,
            <<"value">> => <<"2008-12-10">>
        },
        <<"json_datetime">> => #{
            <<"type">> => <<"datetime">>,
            <<"value">> => <<"2008-12-10T11:12:13Z">>
        },
        <<"optional">> => undefined,
        <<"iri">> => {iri, <<"https://example.test/id/1">>},
        <<"resource">> => {rsc, 123},
        <<"json_iri">> => #{
            <<"type">> => <<"iri">>,
            <<"value">> => <<"https://example.test/id/2">>
        },
        <<"json_resource">> => #{
            <<"type">> => <<"resource">>,
            <<"value">> => 456
        }
    }, Context),
    ?assertEqual({value, true, boolean}, maps:get({var, <<"enabled">>}, Arguments)),
    ?assertEqual({value, <<"draft">>, text}, maps:get({var, <<"status">>}, Arguments)),
    ?assertEqual({value, 42, integer}, maps:get({var, <<"count">>}, Arguments)),
    ?assertEqual({value, 1.5, float}, maps:get({var, <<"ratio">>}, Arguments)),
    ?assertEqual({value, {{2008, 12, 10}, {0, 0, 0}}, datetime}, maps:get({var, <<"date">>}, Arguments)),
    ?assertEqual({value, DateTime, datetime}, maps:get({var, <<"datetime">>}, Arguments)),
    ?assertEqual(
        {value, {{2008, 12, 10}, {0, 0, 0}}, datetime},
        maps:get({var, <<"json_date">>}, Arguments)),
    ?assertEqual(
        {value, DateTime, datetime},
        maps:get({var, <<"json_datetime">>}, Arguments)),
    ?assertEqual(undefined, maps:get({var, <<"optional">>}, Arguments)),
    ?assertEqual({iri, <<"https://example.test/id/1">>}, maps:get({var, <<"iri">>}, Arguments)),
    ?assertEqual({resource, 123}, maps:get({var, <<"resource">>}, Arguments)),
    ?assertEqual(
        {iri, <<"https://example.test/id/2">>},
        maps:get({var, <<"json_iri">>}, Arguments)),
    ?assertEqual({resource, 456}, maps:get({var, <<"json_resource">>}, Arguments)).

invalid_argument_test() ->
    Context = test_context(),
    {ok, Query} = z_sparql:parse(<<
        "SELECT ?r WHERE { ?r <https://example.test/name> ?name }"
    >>),
    ?assertEqual(
        {error, {invalid_argument_date, {2008, 13, 10}}},
        z_sparql_plan:to_query_plan(Query, #{ date => {2008, 13, 10} }, Context)),
    ?assertMatch(
        {error, {duplicate_argument, {var, <<"name">>}}},
        z_sparql_plan:to_query_plan(Query, #{ name => first, <<"name">> => second }, Context)).

sql_argument_binding_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:name ?name . "
                    "?person test:count ?count . "
                    "FILTER(?count > ?minimum) "
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, #{ name => published, minimum => 10 }, Context),
            #search_sql{ args = Args, where = Where } = z_search_terms:combine(Terms, Context),
            ?assert(lists:member({term_json, <<"published">>}, Args)),
            ?assert(lists:member(10, Args)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"::jsonb">>))
        end).

undefined_argument_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person ?optional WHERE { "
                    "?person test:name ?name . "
                    "FILTER(?optional = 1) "
                "}"
            >>),
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, #{ name => undefined, optional => undefined }, Context),
            #search_sql{ args = Args, where = Where, select = Select } =
                z_search_terms:combine(Terms, Context),
            ?assertNot(lists:member({term_json, <<"undefined">>}, Args)),
            ?assertEqual(nomatch, binary:match(Where, <<"::jsonb">>)),
            ?assertNotEqual(nomatch, binary:match(Where, <<"NULL = ">>)),
            ?assertNotEqual(nomatch, binary:match(Select, <<"NULL AS sparql_1">>))
        end).

rdf_term_argument_test() ->
    with_observers(
        fun(Context) ->
            {ok, Query} = z_sparql:parse(<<
                "PREFIX test: <https://example.test/> "
                "SELECT ?person WHERE { "
                    "?person test:name ?name . "
                    "FILTER(isIRI(?iri)) "
                    "FILTER(isIRI(?resource)) "
                    "FILTER(!isBLANK(?resource)) "
                "}"
            >>),
            Arguments = #{
                iri => {iri, <<"https://example.test/id/1">>},
                resource => {rsc, 123}
            },
            {ok, Terms} = z_sparql_sql:to_sql_term(Query, Arguments, Context),
            #search_sql{ where = Where } = z_search_terms:combine(Terms, Context),
            ?assertEqual(2, length(binary:matches(Where, <<"true">>))),
            ?assertEqual(1, length(binary:matches(Where, <<"false">>)))
        end).


with_observers(Fun) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ok = z_notifier:observe(rdf_ns, {?MODULE, observe_rdf_ns}, 100, Context),
    ok = z_notifier:observe(sparql_mapping, {?MODULE, observe_sparql_mapping}, 100, Context),
    try
        Fun(Context)
    after
        z_notifier:detach(rdf_ns, Context),
        z_notifier:detach(sparql_mapping, Context)
    end.

test_context() ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    z_acl:sudo(z_context:new(zotonic_site_testsandbox)).

observe_rdf_ns(#rdf_ns{ ns = <<"https://example.test/">> }, _Context) ->
    {ok, <<"test">>};
observe_rdf_ns(#rdf_ns{}, _Context) ->
    undefined.

observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"name">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"name">>], text}};
observe_sparql_mapping(#sparql_mapping{ ns_prefix = <<"test">>, predicate = <<"count">> }, _Context) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [<<"count">>], integer}};
observe_sparql_mapping(#sparql_mapping{}, _Context) ->
    undefined.
