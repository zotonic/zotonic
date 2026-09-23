%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2026 Marc Worrell
%% @doc Verify query privacy and inference resistance against PostgreSQL.
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

-module(z_sparql_privacy_db_tests).

-export([run/1, observe_rdf_ns/2, observe_sparql_mapping/2,
    observe_acl_add_sql_check/2, observe_acl_query_source/2, observe_url_abs/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_sparql/include/sparql.hrl").

compiler_privacy_test_() ->
    {timeout, 60, fun() ->
        ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
        C = z_context:new(zotonic_site_testsandbox),
        ok = z_db:transaction(fun(Tx) ->
            run(fun(Sql, Args) -> z_db:q(Sql, Args, Tx) end)
        end, C)
    end}.

%% @doc Execute on one connection; temporary fixtures never change site resources.
run(Query) ->
    {ok, _} = application:ensure_all_started(zotonic_notifier),
    C = z_context:new(sparql_privacy_fixture),
    Member = C#context{user_id = 123},
    Admin = z_acl:sudo(C),
    Dispatch = ets:new(z_utils:name_for_site(z_dispatcher, C), [named_table, public]),
    Observers = [rdf_ns, sparql_mapping, acl_add_sql_check, acl_query_source, url_abs],
    lists:foreach(fun(N) ->
        F = list_to_atom("observe_" ++ atom_to_list(N)),
        z_notifier:observe(N, {?MODULE, F}, 100, C)
    end, Observers),
    z_notifier:observe(acl_query_prop, {mod_acl_user_groups, observe_acl_query_prop}, 100, C),
    meck:new(z_db_table, [passthrough, no_link]),
    meck:expect(z_db_table, column, fun
        (<<"rsc">>, Column, #context{site = sparql_privacy_fixture}) ->
            case lists:member(Column, [<<"id">>, <<"category_id">>, <<"props_json">>,
                    <<"pivot_date_start">>, <<"pivot_city">>]) of
                true -> {ok, #column_def{}};
                false -> {error, enoent}
            end;
        (Table, Column, Context) -> meck:passthrough([Table, Column, Context])
    end),
    meck:new(m_category, [passthrough, no_link]),
    meck:expect(m_category, contains, fun
        (person, #context{site = sparql_privacy_fixture}) -> [10, 11];
        (Cat, Context) -> meck:passthrough([Cat, Context])
    end),
    Query("CREATE TEMP TABLE rsc (id integer, category_id integer, props_json jsonb, pivot_date_start timestamp)", []),
    Query("INSERT INTO rsc VALUES (1,10,'{\"privacy\":0,\"email\":\"public\",\"billing_email\":\"bill\"}',now()),"
        "(2,10,'{\"privacy\":10,\"email\":\"member\"}',now()),"
        "(3,10,'{\"privacy\":50,\"email\":\"secret\"}',now()),"
        "(4,11,'{\"email\":\"default-private\"}',now()),"
        "(5,12,'{\"email\":\"default-public\"}',now())", []),
    Query("ALTER TABLE pg_temp.rsc ADD COLUMN privacy integer NOT NULL DEFAULT -1", []),
    Query("UPDATE pg_temp.rsc SET privacy = CASE id WHEN 1 THEN 0 WHEN 2 THEN 10 "
        "WHEN 3 THEN 50 WHEN 4 THEN 30 WHEN 5 THEN 0 END", []),
    Query("ALTER TABLE pg_temp.rsc ADD COLUMN pivot_city text", []),
    Query("UPDATE pg_temp.rsc SET pivot_city=props_json->>'email'", []),
    try
        Basic = <<"SELECT ?id WHERE { ?s p:id ?id . ?s p:email ?email } ORDER BY ?id">>,
        ?assertEqual([1,5], ids(Basic, C, Query)),
        ?assertEqual([1,2,5], ids(Basic, Member, Query)),
        ?assertEqual([1,2,3,4,5], ids(Basic, Admin, Query)),
        %% Privacy is a range: intermediate levels are allowed, pending and higher levels are not.
        Query("UPDATE pg_temp.rsc SET privacy=5 WHERE id=2", []),
        ?assertEqual([1,2,5], ids(Basic, Member, Query)),
        ?assertEqual([1,5], ids(Basic, C, Query)),
        Query("UPDATE pg_temp.rsc SET privacy=-1 WHERE id=2", []),
        ?assertEqual([1,5], ids(Basic, Member, Query)),
        Query("UPDATE pg_temp.rsc SET privacy=11 WHERE id=2", []),
        ?assertEqual([1,5], ids(Basic, Member, Query)),
        Query("UPDATE pg_temp.rsc SET privacy=10 WHERE id=2", []),
        ?assertEqual([], ids(<<"SELECT ?id WHERE { ?s p:id ?id . ?s p:billing_email ?email }">>, C, Query)),
        ?assertEqual([1], ids(<<"SELECT ?id WHERE { ?s p:id ?id . ?s p:billing_email ?email }">>, Admin, Query)),
        Optional = <<"SELECT ?id ?email WHERE { ?s p:id ?id OPTIONAL { ?s p:email ?email } } ORDER BY ?id">>,
        OptionalRows = execute(Optional, C, Query),
        ?assertEqual(5, length(OptionalRows)),
        ?assertEqual(null, element(3, lists:nth(3, OptionalRows))),
        Exists = <<"SELECT ?id WHERE { ?s p:id ?id FILTER EXISTS { ?s p:email ?email } } ORDER BY ?id">>,
        ?assertEqual([1,5], ids(Exists, C, Query)),
        NotExists = <<"SELECT ?id WHERE { ?s p:id ?id FILTER NOT EXISTS { ?s p:email ?email } } ORDER BY ?id">>,
        ?assertEqual([2,3,4], ids(NotExists, C, Query)),
        Union = <<"SELECT ?id WHERE { ?s p:id ?id { ?s p:email ?email } UNION { ?s p:id ?other FILTER (?other = 3) } } ORDER BY ?id">>,
        ?assertEqual([1,3,5], ids(Union, C, Query)),
        Count = <<"SELECT (COUNT(?email) AS ?count) WHERE { ?s p:id ?id OPTIONAL { ?s p:email ?email } }">>,
        ?assertEqual(2, element(1, hd(execute(Count, C, Query)))),
        %% Standard search: sort, null filters, shortcut and negation.
        Sort = #{<<"term">> => <<"sort">>, <<"value">> => <<"pivot_date_start">>},
        Null = #{<<"term">> => <<"filter:pivot_date_start">>, <<"value">> => none},
        NotNull = Null#{<<"operator">> => <<"<>">>},
        ?assertEqual([1,5], standard([Sort], C, Query)),
        CitySort = Sort#{<<"value">> => <<"pivot_city">>},
        ?assertEqual([1,5], standard([CitySort], C, Query)),
        ?assertEqual([1,2,3,4,5], standard([CitySort], Admin, Query)),
        ?assertEqual([1,5], standard([NotNull], C, Query)),
        ?assertEqual([1,5], standard([#{<<"operator">> => <<"noneof">>, <<"terms">> => [Null]}], C, Query)),
        ?assertEqual([], standard([#{<<"term">> => <<"filter:props_json">>, <<"value">> => none}], C, Query)),
        Sorted = <<"SELECT ?id ?email WHERE { ?s p:id ?id OPTIONAL { ?s p:email ?email } } ORDER BY ?email ?id">>,
        Cases = [Basic, Optional, Exists, NotExists, Union, Count, Sorted],
        %% Explicit property selectors and automatic non-column filters share ACL.
        Prop = #{<<"term">> => <<"prop:email">>, <<"value">> => <<"secret">>},
        ?assertEqual([], standard([Prop], C, Query)),
        ?assertEqual([3], standard([Prop], Admin, Query)),
        ?assertEqual([], standard([Prop#{<<"term">> => <<"filter:email">>}], C, Query)),
        ?assertEqual([3], standard([Prop#{<<"term">> => <<"filter:email">>}], Admin, Query)),
        ?assertEqual([1,5], standard([Prop#{<<"value">> => [<<"public">>, <<"default-public">>]}], C, Query)),
        ?assertEqual([1,5], standard([#{<<"term">> => <<"sort">>, <<"value">> => <<"prop:email">>}], C, Query)),
        ?assertEqual([], standard([Prop#{<<"term">> => <<"prop:billing_email">>, <<"value">> => <<"bill">>}], C, Query)),
        ?assertEqual([1,5], standard([Prop#{<<"value">> => null, <<"operator">> => <<"<>">>}], C, Query)),
        ?assertEqual([], standard([Prop#{<<"value">> => <<"' OR true --">>}], C, Query)),
        ?assertEqual([], standard([Prop#{<<"term">> => <<"prop:email') OR true --">>}], C, Query)),
        ?assertEqual([1], standard([Prop#{<<"value">> => <<"pub">>, <<"operator">> => <<"~">>}], C, Query)),
        Query("UPDATE pg_temp.rsc SET props_json=props_json || '{\"address_city\":{\"label\":\"hidden\"}}' WHERE id=3", []),
        Nested = Prop#{<<"term">> => <<"prop:address_city.label">>, <<"value">> => <<"hidden">>},
        ?assertEqual([], standard([Nested], C, Query)),
        ?assertEqual([3], standard([Nested], Admin, Query)),
        Query("UPDATE pg_temp.rsc SET props_json=props_json || '{\"score\":10,\"is_ready\":true,\"contact\":{\"city\":\"Delft\"}}'", []),
        ?assertEqual([1,2,3,4,5], standard([#{<<"term">> => <<"prop:score">>, <<"value">> => 9, <<"operator">> => <<">">>}], C, Query)),
        ?assertEqual([1,2,3,4,5], standard([#{<<"term">> => <<"prop:is_ready">>, <<"value">> => true}], C, Query)),
        ?assertEqual([1,2,3,4,5], standard([#{<<"term">> => <<"prop:contact.city">>, <<"value">> => <<"Delft">>}], C, Query)),
        Query("UPDATE pg_temp.rsc SET props_json=jsonb_set(props_json,'{score}',to_jsonb(6-id))", []),
        JsonSort = search_query:search(#{<<"q">> => [#{<<"term">> => <<"sort">>, <<"value">> => <<"prop:score">>}]}, C),
        SortQ = z_search_terms:combine(JsonSort, C),
        {SortSql, SortArgs} = z_search:concat_sql_query(SortQ, undefined),
        ?assertEqual([{5},{4},{3},{2},{1}], Query(SortSql, SortArgs)),
        {CountSql, CountArgs} = z_search:concat_sql_query(SortQ#search_sql{select = <<"count(*)">>, order = <<>>}, undefined),
        ?assertEqual([{5}], Query(CountSql, CountArgs)),
        Before = [execute(Q, C, Query) || Q <- Cases],
        Query("UPDATE rsc SET props_json=jsonb_set(props_json,'{email}','\"changed-hidden\"') WHERE id IN (2,3,4)", []),
        ?assertEqual(Before, [execute(Q, C, Query) || Q <- Cases]),
        Query("UPDATE pg_temp.rsc SET pivot_date_start=null, pivot_city='hidden-city' WHERE id IN (2,3,4)", []),
        ?assertEqual([1,5], standard([NotNull], C, Query)),
        ?assertEqual([1,5], standard([CitySort], C, Query)),
        ok
    after
        Query("DROP TABLE pg_temp.rsc", []),
        ets:delete(Dispatch),
        meck:unload(m_category),
        meck:unload(z_db_table),
        lists:foreach(fun(N) -> z_notifier:detach(N, C) end, [acl_query_prop | Observers])
    end.

ids(Text, C, Query) -> [element(1, R) || R <- execute(Text, C, Query)].

execute(Text, C, Query) ->
    {ok, Parsed} = z_sparql:parse(<<"PREFIX p: <https://privacy.test/> ", Text/binary>>),
    {ok, Terms} = z_sparql_sql:to_sql_term(Parsed, C),
    Q = z_search_terms:combine(Terms, C),
    {Sql, Args} = z_search:concat_sql_query(Q, undefined),
    Query(Sql, Args).

standard(Terms, C, Query) ->
    Search = search_query:search(#{<<"q">> => Terms}, C),
    Q = z_search_terms:combine(Search, C),
    {Sql, Args} = z_search:concat_sql_query(Q#search_sql{order = <<"rsc.id">>}, undefined),
    [Id || {Id} <- Query(Sql, Args)].

observe_rdf_ns(#rdf_ns{ns = <<"https://privacy.test/">>}, _) -> {ok, <<"p">>};
observe_rdf_ns(_, _) -> undefined.
observe_sparql_mapping(#sparql_mapping{ns_prefix = <<"p">>, predicate = <<"id">>}, _) ->
    {ok, {column, <<"rsc">>, <<"id">>, id}};
observe_sparql_mapping(#sparql_mapping{ns_prefix = <<"p">>, predicate = Property}, _) ->
    {ok, {jsonb, <<"rsc">>, <<"props_json">>, [Property], text}};
observe_sparql_mapping(_, _) -> undefined.
observe_acl_add_sql_check(#acl_add_sql_check{args = Args}, _) -> {[], Args}.
observe_acl_query_source(#acl_query_source{}, _) -> undefined.

observe_url_abs(#url_abs{url = Url}, _) -> <<"https://privacy.test", Url/binary>>.
