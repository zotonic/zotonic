%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @hidden

-module(m_identity_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

hash_test() ->
    ?assertMatch({bcrypt, <<"$2a$12$", _/binary>>}, m_identity:hash("1234")),
    ?assertMatch({bcrypt, <<"$2a$12$", _/binary>>}, m_identity:hash(<<"1234">>)),
    ?assertMatch({bcrypt, <<"$2a$12$", _/binary>>}, m_identity:hash(<<195, 154, 105, 105>>)),
    ok.

needs_rehash_test() ->
    ?assertEqual(true, m_identity:needs_rehash(old_hash("password"))),
    ?assertEqual(false, m_identity:needs_rehash(m_identity:hash("password"))),
    ok.

hash_is_equal_test() ->
    PasswordHash = m_identity:hash("password"),

    ?assert(m_identity:hash_is_equal("password", PasswordHash)),
    ?assert(not m_identity:hash_is_equal("1234", PasswordHash)),

    ok.

hash_is_equal_old_hash_test() ->
    PasswordHash = old_hash("password"),

    % Validating stored old password hashes should still work.
    ?assert(m_identity:hash_is_equal("password", PasswordHash)),
    ?assert(not m_identity:hash_is_equal("1234", PasswordHash)),

    ok.

check_password_no_user_test() ->
    ok = z_sites_manager:await_startup(testsandboxdb),
    C = z_context:new(testsandboxdb),
    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),
    ok = delete_user("mr_z", AdminC),

    ?assertEqual({error, nouser}, m_identity:check_username_pw("mr_z", "does-not-exist", C)),
    ok.

check_username_password_test_() ->
    {timeout, 20, fun() -> check_username_password() end}.

check_username_password() ->
    ok = z_sites_manager:await_startup(testsandboxdb),
    C = z_context:new(testsandboxdb),
    start_modules(C),

    AdminC = z_acl:logon(?ACL_ADMIN_USER_ID, C),

    MrYId = ensure_new_user("mr_y", "secret", AdminC),

    ?assertEqual({ok, MrYId}, m_identity:check_username_pw("mr_y", "secret", C)),
    ?assertEqual({error, password}, m_identity:check_username_pw("mr_y", "wrong-secret", C)),

    %% Make sure stored hashes don't need to be upgraded.
    {MrYId, Hash} = z_db:q_row("select rsc_id, propb from identity where type = 'username_pw' and key = $1",
        ["mr_y"], C),
    ?assert(not m_identity:needs_rehash(Hash)),

    %% update the hash value in the database and set the old hash algorithm.
    OldHash = old_hash("secret"),
    z_db:q("update identity set propb = $2 where type = 'username_pw' and key = $1",
        ["mr_y", {term, OldHash}], C),

    {MrYId, CurrentHash} = z_db:q_row(
        "select rsc_id, propb from identity where type = 'username_pw' and key = $1", ["mr_y"], C),
    ?assert(m_identity:needs_rehash(CurrentHash)),

    %% Logging in with the wrong password still does not work.
    ?assertEqual({error, password}, m_identity:check_username_pw("mr_y", "wrong-secret", C)),

    %% And with the correct password you can login.
    ?assertEqual({ok, MrYId}, m_identity:check_username_pw("mr_y", "secret", C)),

    %% But now hash of the user has been replaced with one which does not need a rehash
    {MrYId, NewHash} = z_db:q_row(
        "select rsc_id, propb from identity where type = 'username_pw' and key = $1", ["mr_y"], C),
    ?assert(not m_identity:needs_rehash(NewHash)),

    %% And afterwards the user can still logon
    ?assertEqual({error, password}, m_identity:check_username_pw("mr_y", "wrong-secret", C)),
    ?assertEqual({ok, MrYId}, m_identity:check_username_pw("mr_y", "secret", C)),

    ok.

start_modules(Context) ->
    ok = z_module_manager:activate_await(mod_acl_mock, Context),
    ok = z_module_manager:activate_await(mod_authentication, Context),
    ok = z_module_manager:activate_await(mod_admin, Context),
    ok = z_module_manager:activate_await(mod_admin_identity, Context),
    ok = z_module_manager:await_upgrade(Context).


%% Old hash algorithm copied from m_identity before the change to bcrypt.
old_hash(Pw) ->
    Salt = binary_to_list(z_ids:id(10)),
    Hash = crypto:hash(sha, [Salt, Pw]),
    {hash, Salt, Hash}.


ensure_new_user(Name, Password, Context) ->
    z_db:transaction(fun(Ctx) ->
        ok = delete_user(Name, Ctx),

        PersonId = m_rsc:rid(person, Ctx),
        {ok, Id} = m_rsc:insert([{title, Name}, {category_id, PersonId}], Ctx),

        ok = m_identity:set_username_pw(Id, Name, Password, Ctx),
        Id
    end, Context).


delete_user(Name, Context) ->
    case m_identity:lookup_by_username(Name, Context) of
        undefined -> ok;
        Props ->
            {rsc_id, Id} = proplists:lookup(rsc_id, Props),
            m_rsc:delete(Id, Context)
    end.
