-module(m_rsc_import_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


modify_rsc_test() ->
    C = z_context:new(testsandbox),
    AdminC = z_acl:sudo(C),

    ?assertThrow({error, eacces}, m_rsc_import:create_empty("http://foo.com/id/333", C)),

    {ok, Id} = m_rsc_import:create_empty("http://foo.com/id/333", AdminC),

    ?assertEqual(Id, m_rsc:uri_lookup("http://foo.com/id/333", C)),
    ?assertEqual(Id, m_rsc:uri_lookup(<<"http://foo.com/id/333">>, C)),

    ?assertThrow({error, duplicate_uri}, m_rsc_import:create_empty("http://foo.com/id/333", AdminC)),

    %% Existence check
    ?assertEqual(true, m_rsc:exists(Id, AdminC)),
    ?assertEqual(true, m_rsc:exists(Id, C)),

    %% Check properties
    ?assertEqual(undefined, m_rsc:p(Id, title, AdminC)),
    ?assertEqual(1, m_rsc:p(Id, version, AdminC)),
    ?assertEqual(false, m_rsc:p(Id, is_published, AdminC)),
    ?assertEqual(false, m_rsc:p(Id, is_authoritative, AdminC)),
    ?assertEqual(true, m_rsc:is_a(Id, other, AdminC)),
    ?assertEqual(<<"http://foo.com/id/333">>, m_rsc:p(Id, uri, AdminC)),

    %% Cannot update a non-authoritative resource
    ?assertThrow({error, non_authoritative}, m_rsc:update(Id, [{title, <<"foo">>}], AdminC)),


    RscImport = [{uri, <<"http://foo.com/id/333">>},
                 {rsc, [{title, <<"Hello!">>},
                        {summary, <<"This is the summary.">>},
                        {body, <<"This is a <strong>statement</strong>.">>}
                       ]}
                ],
    ?assertThrow({error, eacces}, m_rsc_import:import(RscImport, C)),

    {ok, NewId} = m_rsc_import:import(RscImport, AdminC),
    ?assertEqual(Id, NewId),

    ?assertEqual(<<"Hello!">>, m_rsc:p(Id, title, AdminC)),
    ?assertEqual(2, m_rsc:p(Id, version, AdminC)),
    ?assertEqual(true, m_rsc:p(Id, is_published, AdminC)), % once imported, it is published

    ok.

