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

    lager:info("[~p] Expecting duplicate_uri error...", [?MODULE]),
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
    ?assertEqual(1, m_rsc:p(Id, version, AdminC)),
    ?assertEqual(true, m_rsc:p(Id, is_published, AdminC)),
    ?assertEqual(false, m_rsc:p(Id, is_authoritative, AdminC)),
    ?assertEqual(<<"https://foo.test/id/333">>, m_rsc:p(Id, uri, AdminC)),
    ?assertEqual({{2014,4,30},{22,0,0}}, m_rsc:p(Id, created, AdminC)),
    ?assertEqual({{2021,7,14},{8,47,7}}, m_rsc:p(Id, modified, AdminC)),

    %% Check edges
    ?assertEqual([1], m_edge:objects(Id, author, AdminC)),

    %% Usual rights
    ?assertEqual(false, z_acl:rsc_editable(Id, C)),
    ?assertEqual(true, z_acl:rsc_editable(Id, AdminC)),

    ok.

export_data() ->
    #{<<"depiction_url">> => <<"https://localhost/lib/images/koe.jpg">>,
      <<"edges">> =>
          #{<<"author">> =>
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
                        <<"uri">> => <<"http://purl.org/dc/terms/creator">>}}},
      <<"id">> => 333,
      <<"is_a">> => [ <<"text">>, <<"article">>, <<"foobartext">> ],
      <<"resource">> =>
          #{<<"version">> => 1,
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


