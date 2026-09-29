%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(m_rsc_import_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("../include/zotonic.hrl").

modify_rsc_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:upgrade_await(C),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),

    %% cleanup eventual remains of earlier failed tests
    case m_rsc:uri_lookup("https://foo.test/id/333", AdminC) of
        undefined -> nop;
        ExistingId -> ?assertEqual(ok, m_rsc:delete(ExistingId, AdminC))
    end,

    Data = export_data(),

    %% perform the tests
    ?assertEqual({error, eacces}, m_rsc_import:import(Data, C)),

    Options = [
        {import_edges, 1}
    ],
    {ok, {Id, _}} = m_rsc_import:import(Data, Options, AdminC),

    ?assertEqual(Id, m_rsc:uri_lookup("https://foo.test/id/333", C)),
    ?assertEqual(Id, m_rsc:rid(<<"https://foo.test/id/333">>, C)),
    ?assertEqual(Id, m_rsc:rid(#{ <<"uri">> => <<"https://foo.test/id/333">> }, C)),

    ?LOG_NOTICE(#{
        text => <<"Expecting duplicate_uri error...">>,
        in => zotonic_core,
        module => ?MODULE
    }),
    DupRsc = #{
        <<"uri">> => <<"https://foo.test/id/333">>,
        <<"category_id">> => <<"person">>,
        <<"title">> => <<"Dup Person">>
    },
    ?assertEqual({error, duplicate_uri}, m_rsc:insert(DupRsc, AdminC)),

    % Reimporting the same, should return the same id.
    % No edge import should keep the existing edges as well.
    {ok, {Id, _}} = m_rsc_import:import(Data, [], AdminC),

    %% Existence check
    ?assertEqual(true, m_rsc:exists(Id, AdminC)),
    ?assertEqual(true, m_rsc:exists(Id, C)),

    %% Check properties
    ?assertEqual(<<"Hello World">>, m_rsc:p(Id, title, AdminC)),
    ?assertEqual(true, m_rsc:p(Id, is_published, AdminC)),
    ?assertEqual(false, m_rsc:p(Id, is_authoritative, AdminC)),
    ?assertEqual(<<"https://foo.test/id/333">>, m_rsc:p(Id, uri, AdminC)),
    ?assertEqual({{2014,4,30},{22,0,0}}, m_rsc:p(Id, created, AdminC)),
    ?assertEqual({{2021,7,14},{8,47,7}}, m_rsc:p(Id, modified, AdminC)),

    ?assertEqual(<<"rsc_import_test">>, m_rsc:p(Id, name, AdminC)),
    ?assertEqual(<<"/rsc-import-test">>, m_rsc:p(Id, page_path, AdminC)),


    %% Check edges
    ?assertEqual([1], m_edge:objects(Id, author, AdminC)),
    ?assertEqual([Id, 1], m_edge:objects(Id, relation, AdminC)),

    %% Usual rights
    ?assertEqual(false, z_acl:rsc_editable(Id, C)),
    ?assertEqual(true, z_acl:rsc_editable(Id, AdminC)),


    % Import again, now as a local authoritative copy
    OptionsAuth = [
        {import_edges, 1},
        is_authoritative
    ],
    {ok, {IdAuth, _}} = m_rsc_import:import(Data, OptionsAuth, AdminC),

    %% Must be a different resource than the previous import.
    ?assertEqual(true, Id =/= IdAuth),
    ?assertEqual(true, m_rsc:p(IdAuth, is_published, AdminC)),
    ?assertEqual(true, m_rsc:p(IdAuth, is_authoritative, AdminC)),

    %% Check edges
    ?assertEqual([1], m_edge:objects(IdAuth, author, AdminC)),
    ?assertEqual([IdAuth, 1], m_edge:objects(IdAuth, relation, AdminC)),

    % The collision on page_path and name should have renamed those
    % properties from the import.
    ?assertEqual(<<"rsc_import_test_1">>, m_rsc:p(IdAuth, name, AdminC)),
    ?assertEqual(<<"/rsc-import-test-1">>, m_rsc:p(IdAuth, page_path, AdminC)),

    ok.

%% Test that page_path is properly escaped on insert
page_path_test() ->
    C = z_context:new(zotonic_site_testsandbox),
    ok = z_module_manager:upgrade_await(C),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    Rsc1 = #{
        category_id => other,
        title => <<"Page Path Test">>,
        language => [ en ],
        is_published => true,
        page_path => #trans{ tr = [
            {en, <<"/på/öl"/utf8>>}
        ]}
    },
    {ok, Id1} = m_rsc:insert(Rsc1, [ {is_escape_texts, true} ], AdminC),
    ?assertEqual(#trans{ tr = [{en, <<"/p%C3%A5/%C3%B6l">>}] }, m_rsc:p(Id1, <<"page_path">>, AdminC)),
    ok = m_rsc:delete(Id1, AdminC),
    Rsc2 = #{
        category_id => other,
        title => <<"Page Path Test">>,
        language => [ en ],
        is_published => true,
        page_path => #trans{ tr = [
            {en, <<"/p%C3%A5/%C3%B6l">>}
        ]}
    },
    {ok, Id2} = m_rsc:insert(Rsc2, [ {is_escape_texts, false} ], AdminC),
    ?assertEqual(#trans{ tr = [{en, <<"/p%C3%A5/%C3%B6l">>}] }, m_rsc:p(Id2, <<"page_path">>, AdminC)),
    ok = m_rsc:delete(Id2, AdminC),
    ok.

%% Exercise both direct imports and reference placeholders across successive
%% deletions of the same semantic URI. Being dependent alone is not permission
%% to resurrect a resource: only automatic cleanup records that reason.
deleted_references_test() ->
    C = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    Suffix = integer_to_binary(erlang:unique_integer([positive, monotonic])),
    ParentUri = <<"https://deleted-import.test/parent/", Suffix/binary>>,
    ChildUri = <<"https://deleted-import.test/child/", Suffix/binary>>,
    Child = #{
        <<"uri">> => ChildUri,
        <<"is_a">> => [ <<"text">> ],
        <<"title">> => <<"Deleted reference">>
    },
    Data = #{
        <<"id">> => 42,
        <<"uri">> => ParentUri,
        <<"uri_template">> => <<"https://deleted-import.test/id/:id">>,
        <<"is_a">> => [ <<"text">> ],
        <<"resource">> => #{ <<"title">> => <<"Deletion lifecycle">> },
        <<"edges">> => #{ <<"relation">> => #{
            <<"predicate">> => #{ <<"name">> => <<"relation">> },
            <<"objects">> => [ #{ <<"object_id">> => Child } ]
        } }
    },
    Options = [{import_edges, 1}, {is_forced_update, true}],
    try
        {ok, {ParentId, _}} = m_rsc_import:import(Data, Options, C),
        [ChildId] = m_edge:objects(ParentId, relation, C),
        {ok, ChildId} = m_rsc:update(ChildId, #{ <<"is_dependent">> => true }, C),
        ok = m_rsc:delete(ChildId, C),
        ?assert(m_rsc_gone:is_import_blocked(ChildUri, C)),
        {ok, {ParentId, ImportMap}} = m_rsc_import:import(Data, Options, C),
        ?assertEqual([], m_edge:objects(ParentId, relation, C)),
        ?assertNot(maps:is_key(ChildUri, ImportMap)),
        ?assertEqual(undefined, m_rsc:uri_lookup(ChildUri, C)),
        ChildData = Data#{ <<"uri">> => ChildUri, <<"edges">> => #{} },
        ?assertEqual({error, deleted}, m_rsc_import:import(ChildData, Options, C)),

        %% Explicit override creates a new resource and saves the preference.
        AllOptions = [is_import_deleted | Options],
        {ok, {ParentId, _}} = m_rsc_import:import(Data, AllOptions, C),
        [RestoredId] = m_edge:objects(ParentId, relation, C),
        ?assert(RestoredId =/= ChildId),
        {ok, Status} = m_rsc_import:get_import_status(ParentId, C),
        ?assert(proplists:get_value(is_import_deleted, maps:get(<<"options">>, Status))),

        %% Automatic removal of an unconnected dependent item allows revival.
        {ok, RestoredId} = m_rsc:update(RestoredId, #{ <<"is_dependent">> => true }, C),
        ok = m_edge:set_sequence(ParentId, relation, [], C),
        ok = z_edge_log_server:delete_if_unconnected(RestoredId, C),
        ?assertNot(m_rsc:exists(RestoredId, C)),
        ?assert(m_rsc_gone:is_gone_uri(ChildUri, C)),
        ?assertNot(m_rsc_gone:is_import_blocked(ChildUri, C)),
        {ok, {ParentId, _}} = m_rsc_import:import(Data, Options, C),
        [RecreatedId] = m_edge:objects(ParentId, relation, C),
        ?assert(RecreatedId =/= RestoredId),
        ok = m_rsc:delete(RecreatedId, C),
        ?assert(m_rsc_gone:is_import_blocked(ChildUri, C)),
        {ok, {ParentId, _}} = m_rsc_import:import(Data, Options, C),
        ?assertEqual([], m_edge:objects(ParentId, relation, C))
    after
        lists:foreach(
            fun(Uri) ->
                case m_rsc:uri_lookup(Uri, C) of
                    undefined -> ok;
                    Id -> m_rsc:delete(Id, C)
                end,
                GoneIds = z_db:q("select id from rsc_gone where uri = $1", [Uri], C),
                lists:foreach(fun({Id}) -> m_rsc_gone:delete(Id, C) end, GoneIds)
            end,
            [ParentUri, ChildUri])
    end.

export_data() ->
    #{<<"depiction_url">> => <<"https://localhost/lib/images/koe.jpg">>,
      <<"edges">> =>
          #{
            <<"author">> =>
                #{<<"objects">> =>
                      [#{<<"created">> => {{2018,11,23},{10,48,11}},
                         <<"object_id">> =>
                             #{<<"id">> => 123456,
                               <<"is_a">> => [ <<"person">> ],
                               <<"name">> => undefined,
                               <<"title">> => <<"Admin">>,
                               <<"uri">> => <<"https://localhost/id/1">>},
                         <<"seq">> => 1}],
                  <<"predicate">> =>
                      #{<<"id">> => 301,
                        <<"is_a">> => [ <<"meta">>, <<"predicate">> ],
                        <<"name">> => <<"author">>,
                        <<"title">> => #trans{ tr = [ {en,<<"Author">>} ]},
                        <<"uri">> => <<"http://purl.org/dc/terms/creator">>}},
            <<"relation">> =>
                #{<<"objects">> =>
                      [#{<<"created">> => {{2018,11,23},{10,48,11}},
                         <<"object_id">> =>
                             #{<<"id">> => 333,
                               <<"is_a">> => [ <<"text">>, <<"article">>, <<"foobartext">> ],
                               <<"name">> => undefined,
                               <<"title">> => <<"Hello World">>,
                               <<"uri">> => <<"https://foo.test/id/333">>},
                         <<"seq">> => 1},
                        #{<<"created">> => {{2018,11,23},{10,48,10}},
                         <<"object_id">> =>
                             #{<<"id">> => 1,
                               <<"is_a">> => [ <<"person">> ],
                               <<"name">> => undefined,
                               <<"title">> => <<"Admin">>,
                               <<"uri">> => <<"https://localhost/id/1">>},
                         <<"seq">> => 2}],
                  <<"predicate">> =>
                      #{<<"id">> => 301,
                        <<"is_a">> => [ <<"meta">>, <<"predicate">> ],
                        <<"name">> => <<"relation">>,
                        <<"title">> => #trans{ tr = [ {en,<<"Related">>} ]},
                        <<"uri">> => <<"http://purl.org/dc/terms/relation">>}}
            },
      <<"id">> => 333,
      <<"is_a">> => [ <<"text">>, <<"article">>, <<"foobartext">> ],
      <<"resource">> =>
          #{<<"version">> => 1,
            <<"name">> => <<"rsc_import_test">>,
            <<"page_path">> => <<"/rsc-import-test">>,
            <<"is_published">> => true,
            <<"is_authoritative">> => true,
            <<"org_pubdate">> => {{2014,4,30},{22,0,0}},
            <<"slug">> => <<"hello-world">>,
            <<"visible_for">> => 0,
            <<"blocks">> => [],
            <<"tz">> => <<"Europe/Berlin">>,
            <<"modified">> => {{2021,7,14},{8,47,7}},
            <<"created">> => {{2014,4,30},{22,0,0}},
            <<"seo_noindex">> => false,
            <<"date_is_all_day">> => true,
            <<"title">> => <<"Hello World">>},
      <<"uri">> => <<"https://foo.test/id/333">>,
      <<"uri_template">> => <<"https://foo.test/id/:id">>
}.
