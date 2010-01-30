-module(m_rsc_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").


modify_rsc_test() ->
    C = z_context:new(testsandbox),
    AdminC = z_acl:sudo(C),

    GroupId = m_rsc:rid(admins, C),
    CatId = m_rsc:rid(text, C),

    ?assertThrow({error, e_user_without_group}, m_rsc:insert([{title, "Hello."}, {category_id, CatId}], C)),
    ?assertThrow({error, eacces}, m_rsc:insert([{title, "Hello."}, {group_id, GroupId}], C)),

    ?assertThrow({error, eacces}, m_rsc:insert([{title, "Hello."}, {group_id, GroupId}, {category_id, CatId}], C)),

    {ok, Id} = m_rsc:insert([{title, "Hello."}, {group_id, GroupId}, {category_id, CatId}], AdminC),

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
    ?assertThrow({error, eacces}, m_rsc:update(Id, [{title, "Bye."}, {is_published, true}], C)),

    {ok, Id} = m_rsc:update(Id, [{title, "Bye."}, {is_published, true}], AdminC),
    ?assertEqual(<<"Bye.">>, m_rsc:p(Id, title, AdminC)),

    ?assertEqual(2, m_rsc:p(Id, version, AdminC)),
    ?assertEqual(true, m_rsc:p(Id, is_published, AdminC)),
    ?assertEqual(<<"Bye.">>, m_rsc:p(Id, title, AdminC)),
    ?assertEqual(<<"Bye.">>, m_rsc:p(Id, title, C)), %% also visible for anonymous now


    %% Delete
    ?assertThrow({error, eacces}, m_rsc:delete(Id, C)),
    ?assertEqual(ok, m_rsc:delete(Id, AdminC)),
    
    %% verify that it's gone
    ?assertEqual(undefined, m_rsc:p(Id, title, AdminC)),

    %% Existence check
    ?assertEqual(false, m_rsc:exists(Id, AdminC)),
    ?assertEqual(false, m_rsc:exists(Id, C)),

    ok.

