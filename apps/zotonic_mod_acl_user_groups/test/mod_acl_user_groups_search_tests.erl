%% @copyright 2026 Marc Worrell
%% @doc End-to-end search ACL tests for mod_search and mod_sparql.
%% @end

-module(mod_acl_user_groups_search_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


mod_search_content_group_acl_test() ->
    with_acl_fixture(
        fun(AllowedContentGroupId, DeniedContentGroupId, UserContext, SudoContext) ->
            with_resources(
                [
                    article_props(<<"Allowed mod_search article">>, AllowedContentGroupId),
                    article_props(<<"Denied mod_search article">>, DeniedContentGroupId)
                ],
                SudoContext,
                fun([AllowedId, DeniedId]) ->
                    SearchArgs = #{
                        <<"q">> => [#{
                            <<"term">> => <<"id">>,
                            <<"value">> => [AllowedId, DeniedId]
                        }]
                    },
                    #search_sql_terms{} = Terms = mod_search:observe_search_query(
                        #search_query{
                            name = <<"query">>,
                            args = SearchArgs,
                            offsetlimit = {1, 20}
                        },
                        UserContext),
                    Query0 = z_search_terms:combine(Terms, UserContext),
                    Query1 = z_search:reformat_sql_query(Query0, #{}, UserContext),
                    assert_content_group_acl(
                        Query1,
                        AllowedContentGroupId,
                        DeniedContentGroupId),

                    #search_result{ result = Result } = z_search:search(
                        <<"query">>, SearchArgs, 1, 20, UserContext),
                    ?assertEqual([AllowedId], Result)
                end)
        end).

mod_sparql_content_group_acl_test() ->
    with_acl_fixture(
        fun(AllowedContentGroupId, DeniedContentGroupId, UserContext, SudoContext) ->
            with_resources(
                [
                    article_props(<<"Allowed SPARQL object">>, AllowedContentGroupId),
                    article_props(<<"Denied SPARQL object">>, DeniedContentGroupId),
                    article_props(
                        <<"SPARQL subject with allowed object">>,
                        AllowedContentGroupId),
                    article_props(
                        <<"SPARQL subject with denied object">>,
                        AllowedContentGroupId)
                ],
                SudoContext,
                fun([AllowedObjectId, DeniedObjectId, AllowedSubjectId, DeniedSubjectId]) ->
                    {ok, _} = m_edge:insert(
                        AllowedSubjectId, relation, AllowedObjectId, SudoContext),
                    {ok, _} = m_edge:insert(
                        DeniedSubjectId, relation, DeniedObjectId, SudoContext),
                    Sparql = sparql_query(AllowedObjectId, DeniedObjectId),
                    {ok, ParsedQuery} = z_sparql:parse(Sparql),
                    {ok, SqlTerms} = z_sparql_sql:to_sql_term(
                        ParsedQuery,
                        UserContext),
                    Query0 = z_search_terms:combine(SqlTerms, UserContext),
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Query0#search_sql.where, <<"EXISTS (">>)),
                    assert_content_group_acl(
                        Query0,
                        AllowedContentGroupId,
                        DeniedContentGroupId),

                    {ok, #search_result{ result = Result }} = z_sparql:search(
                        Sparql,
                        {1, 20},
                        UserContext),
                    ?assertEqual([AllowedSubjectId], Result)
                end)
        end).

mod_sparql_optional_content_group_acl_test() ->
    with_acl_fixture(
        fun(AllowedContentGroupId, DeniedContentGroupId, UserContext, SudoContext) ->
            with_resources(
                [
                    article_props(<<"Allowed OPTIONAL object">>, AllowedContentGroupId),
                    article_props(<<"Denied OPTIONAL object">>, DeniedContentGroupId),
                    article_props(<<"Subject with allowed OPTIONAL">>, AllowedContentGroupId),
                    article_props(<<"Subject with denied OPTIONAL">>, AllowedContentGroupId)
                ],
                SudoContext,
                fun([AllowedObjectId, DeniedObjectId, AllowedSubjectId, DeniedSubjectId]) ->
                    {ok, _} = m_edge:insert(
                        AllowedSubjectId, relation, AllowedObjectId, SudoContext),
                    {ok, _} = m_edge:insert(
                        DeniedSubjectId, relation, DeniedObjectId, SudoContext),
                    Sparql = optional_sparql_query(
                        AllowedSubjectId, DeniedSubjectId, SudoContext),
                    {ok, ParsedQuery} = z_sparql:parse(Sparql),
                    {ok, SqlTerms} = z_sparql_sql:to_sql_term(
                        ParsedQuery,
                        UserContext),
                    Query0 = z_search_terms:combine(SqlTerms, UserContext),
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Query0#search_sql.from, <<"left join LATERAL (SELECT">>)),
                    ?assertNotEqual(
                        nomatch,
                        binary:match(Query0#search_sql.from, <<".content_group_id">>)),
                    ?assertEqual(
                        nomatch,
                        binary:match(Query0#search_sql.where, <<".content_group_id">>)),
                    FlatArgs = lists:flatten(Query0#search_sql.args),
                    ?assert(lists:member(AllowedContentGroupId, FlatArgs)),
                    ?assertNot(lists:member(DeniedContentGroupId, FlatArgs)),

                    {ok, #search_result{ result = Result }} = z_sparql:search(
                        Sparql,
                        {1, 20},
                        UserContext),
                    ?assertEqual(
                        lists:sort([
                            {AllowedSubjectId, AllowedObjectId},
                            {DeniedSubjectId, undefined}
                        ]),
                        lists:sort(Result))
                end)
        end).

sparql_query(AllowedObjectId, DeniedObjectId) ->
    AllowedId = integer_to_binary(AllowedObjectId),
    DeniedId = integer_to_binary(DeniedObjectId),
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?subject WHERE {\n"
        "    ?subject zotonic:id ?subject_id .\n"
        "    {\n"
        "        ?subject dcterms:relation ?object_a .\n"
        "        ?object_a zotonic:id ", AllowedId/binary, "\n"
        "    } UNION {\n"
        "        ?subject dcterms:relation ?object_b .\n"
        "        ?object_b zotonic:id ", DeniedId/binary, "\n"
        "    }\n"
        "}"
    >>.

optional_sparql_query(AllowedSubjectId, DeniedSubjectId, Context) ->
    AllowedSubjectUri = m_rsc:uri(AllowedSubjectId, Context),
    DeniedSubjectUri = m_rsc:uri(DeniedSubjectId, Context),
    <<
        "PREFIX dcterms: <http://purl.org/dc/terms/>\n"
        "PREFIX zotonic: <http://zotonic.net/predicate/>\n"
        "SELECT ?subject ?object WHERE {\n"
        "    ?subject zotonic:id ?subject_id .\n"
        "    VALUES ?subject {\n"
        "        <", AllowedSubjectUri/binary, ">\n"
        "        <", DeniedSubjectUri/binary, ">\n"
        "    }\n"
        "    OPTIONAL { ?subject dcterms:relation ?object }\n"
        "}"
    >>.

article_props(Title, ContentGroupId) ->
    #{
        <<"category">> => article,
        <<"content_group_id">> => ContentGroupId,
        <<"title">> => Title,
        <<"is_published">> => true
    }.

assert_content_group_acl(
        #search_sql{ where = Where, args = Args },
        AllowedContentGroupId,
        DeniedContentGroupId) ->
    ?assertNotEqual(nomatch, binary:match(Where, <<".content_group_id">>)),
    FlatArgs = lists:flatten(Args),
    ?assert(lists:member(AllowedContentGroupId, FlatArgs)),
    ?assertNot(lists:member(DeniedContentGroupId, FlatArgs)).

with_acl_fixture(Fun) ->
    Context = context(),
    SudoContext = z_acl:sudo(Context),
    with_resources(
        [
            #{
                <<"category">> => content_group,
                <<"title">> => <<"Allowed search test content group">>,
                <<"is_published">> => true
            },
            #{
                <<"category">> => content_group,
                <<"title">> => <<"Denied search test content group">>,
                <<"is_published">> => true
            },
            #{
                <<"category">> => acl_user_group,
                <<"title">> => <<"Search test ACL user group">>,
                <<"is_published">> => true
            },
            #{
                <<"category">> => person,
                <<"title">> => <<"Search ACL test user">>,
                <<"is_published">> => true
            }
        ],
        SudoContext,
        fun([AllowedContentGroupId, DeniedContentGroupId, UserGroupId, UserId]) ->
            {ok, _} = m_hierarchy:ensure(acl_user_group, SudoContext),
            {ok, _} = m_edge:insert(
                UserId, hasusergroup, UserGroupId, SudoContext),
            with_managed_rules(
                [
                    {rsc, [
                        {acl_user_group_id, UserGroupId},
                        {content_group_id, AllowedContentGroupId},
                        {category_id, article},
                        {actions, [view]}
                    ]}
                ],
                Context,
                fun() ->
                    UserContext = z_acl:logon(UserId, Context),
                    Fun(
                        AllowedContentGroupId,
                        DeniedContentGroupId,
                        UserContext,
                        SudoContext)
                end)
        end).

context() ->
    Context = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:activate_await(mod_content_groups, Context),
    ok = z_module_manager:activate_await(mod_acl_user_groups, Context),
    ok = z_module_manager:activate_await(mod_search, Context),
    ok = z_module_manager:activate_await(mod_sparql, Context),
    ok = z_module_manager:upgrade_await(Context),
    Context.

with_managed_rules(Rules, Context, Fun) ->
    replace_managed(Rules, Context),
    try
        Fun()
    after
        replace_managed([], Context)
    end.

replace_managed(Rules, Context) ->
    SudoContext = z_acl:sudo(Context),
    ok = m_acl_rule:replace_managed(
        Rules,
        ?MODULE,
        SudoContext),
    await_acl_rebuild(SudoContext).

with_resources(PropsList, Context, Fun) ->
    with_resources(PropsList, Context, [], Fun).

with_resources([], _Context, Ids, Fun) ->
    Fun(lists:reverse(Ids));
with_resources([Props | Rest], Context, Ids, Fun) ->
    {ok, Id} = m_rsc:insert(Props, Context),
    try
        with_resources(Rest, Context, [Id | Ids], Fun)
    after
        ok = m_rsc:delete(Id, Context)
    end.

await_acl_rebuild(Context) ->
    await_acl_rebuild(1000, Context).

await_acl_rebuild(0, _Context) ->
    error(acl_rebuild_timeout);
await_acl_rebuild(N, Context) ->
    {ok, Status} = mod_acl_user_groups:status(Context),
    case {
        proplists:get_value(is_rebuilding, Status),
        proplists:get_value(is_rebuild_publish, Status)
    } of
        {false, false} ->
            ok;
        _ ->
            timer:sleep(10),
            await_acl_rebuild(N - 1, Context)
    end.
