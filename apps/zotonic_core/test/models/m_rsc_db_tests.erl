%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @hidden

-module(m_rsc_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

modify_rsc_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    CatId = m_rsc:rid(text, C),

    ?assertEqual({error, nocategory}, m_rsc:insert([{title, "Hello."}], C)),
    ?assertEqual({error, eacces}, m_rsc:insert([{title, "Hello."}, {category_id, CatId}], C)),

    {ok, Id} = m_rsc:insert([{title, "Hello."}, {category_id, CatId}], AdminC),

    %% Existence check
    ?assertEqual(true, m_rsc:exists(Id, AdminC)),
    ?assertEqual(true, m_rsc:exists(Id, C)),

    %% Check properties
    ?assertEqual(<<"Hello.">>, m_rsc:p(Id, title, AdminC)),
    ?assertEqual(1, m_rsc:p(Id, version, AdminC)),
    ?assertEqual(false, m_rsc:p(Id, is_featured, AdminC)),
    ?assertEqual(true, m_rsc:p(Id, is_authoritative, AdminC)),
    ?assertEqual(true, m_rsc:is_a(Id, text, AdminC)),

    ?assertEqual(false, m_rsc:p(Id, is_published, AdminC)),
    ?assertEqual(undefined, m_rsc:p(Id, title, C)), %% not visible for anonymous yet

    %% Update
    ?assertEqual({error, eacces}, m_rsc:update(Id, [{title, "Bye."}, {is_published, true}], C)),

    {ok, Id} = m_rsc:update(Id, [{title, "Bye."}, {is_published, true}], AdminC),
    ?assertEqual(<<"Bye.">>, m_rsc:p(Id, title, AdminC)),

    ?assertEqual(2, m_rsc:p(Id, version, AdminC)),

    %% Delete
    ?assertEqual({error, eacces}, m_rsc:delete(Id, C)),
    ?assertEqual(ok, m_rsc:delete(Id, AdminC)),

    %% verify that it's gone
    ?assertEqual(undefined, m_rsc:p(Id, title, AdminC)),

    %% Existence check
    ?assertEqual(false, m_rsc:exists(Id, AdminC)),
    ?assertEqual(false, m_rsc:exists(Id, C)),

    ok.


page_path_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),

    {ok, Id} = m_rsc:insert([{title, "Hello."}, {category, text}, {page_path, "/foo/bar"}], AdminC),
    ?assertEqual(<<"/foo/bar">>, m_rsc:p(Id, page_path, AdminC)),
    ok = m_rsc:delete(Id, AdminC).

%% @doc Resource name instead of id as argument.
name_rid_test() ->
    ok = z_sites_manager:await_startup(zotonic_site_testsandbox),
    C = z_context:new(zotonic_site_testsandbox),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    {ok, Id} = m_rsc:insert([{title, <<"What’s in a name?"/utf8>>}, {category_id, text}, {name, rose}],
        AdminC),

    m_rsc:get_raw(rose, AdminC),
    ok = m_rsc_update:flush(rose, AdminC),
    {ok, Id} = m_rsc:update(rose, [], AdminC),
    {ok, _DuplicateId} = m_rsc:duplicate(rose, [], AdminC),
    ok = m_rsc:delete(rose, AdminC).
