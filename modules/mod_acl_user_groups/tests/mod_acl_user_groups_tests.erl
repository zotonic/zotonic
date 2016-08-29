%% @doc Tests for mod_acl_user_groups
-module(mod_acl_user_groups_tests).

-include_lib("eunit/include/eunit.hrl").
-include("zotonic.hrl").

person_can_edit_own_resource_test() ->
    Context = context(),

    %% Person must be able to edit person category
    m_acl_rule:replace_managed(
        [
            {rsc, [
                {acl_user_group_id, acl_user_group_anonymous},
                {actions, [view, update]},
                {is_owner, true},
                {category_id, person}
            ]}
        ],
        ?MODULE,
        z_acl:sudo(Context)
    ),
    %% Wait for ACL rules to be rebuilt
    timer:sleep(100),

    {ok, Id} = m_rsc:insert([{category, person}], z_acl:sudo(Context)),

    %% Must be owner
    ?assertThrow({error, eacces}, m_rsc:update(Id, [{title, <<"Test">>}], Context)),

    UserContext = z_acl:logon(Id, Context),
    {ok, Id} = m_rsc:update(Id, [{title, "Test"}], UserContext).

acl_is_allowed_accepts_rsc_name_object_test() ->
    ?assertEqual(false, acl_user_groups_checks:acl_is_allowed(#acl_is_allowed{action = insert, object = text}, context())).

publish_test() ->
    Context = context(),

    %% Anonymous can view everything
    m_acl_rule:replace_managed(
        [
            {rsc, [
                {acl_user_group_id, acl_user_group_anonymous},
                {actions, [view]}
            ]}
        ],
        ?MODULE,
        z_acl:sudo(Context)
    ),
    %% Wait for ACL rules to be rebuilt
    timer:sleep(100),

    {ok, Id} = m_rsc:insert([{title, <<"Top secret!">>}, {category, text}], z_acl:sudo(Context)),
    ?assertEqual(<<"Top secret!">>, m_rsc:p(Id, title, z_acl:sudo(Context))),
    ?assertEqual(undefined, m_rsc:p(Id, title, Context)), %% invisible for anonymous

    {ok, Id} = m_rsc:update(Id, [{is_published, true}], z_acl:sudo(Context)),
    ?assertEqual(<<"Top secret!">>, m_rsc:p(Id, title, Context)). %% visible for anonymous when published

context() ->
    Context = z_context:new(testsandboxdb),
    start_modules(Context),
    Context.

start_modules(Context) ->
    ok = z_module_manager:activate_await(mod_content_groups, Context),
    ok = z_module_manager:activate_await(mod_acl_user_groups, Context).
