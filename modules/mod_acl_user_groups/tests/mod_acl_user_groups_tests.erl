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
        testsandboxdb,
        z_acl:sudo(Context)
    ),

    {ok, Id} = m_rsc:insert([{category, person}], z_acl:sudo(Context)),

    UserContext = z_acl:logon(Id, Context),
    {ok, Id} = m_rsc:update(Id, [{title, "Test"}], UserContext).

is_allowed_accepts_rsc_name_object_test() ->
    Context = z_context:new(testsandboxdb),
    false = acl_user_groups_checks:acl_is_allowed(#acl_is_allowed{action = view, object = text}, Context).

publish_test() ->
    Context = context(),
    {ok, Id} = m_rsc:insert([{title, <<"Top secret!">>}, {category_id, text}], z_acl:sudo(Context)),
    ?assertEqual(undefined, m_rsc:p(Id, title, Context)), %% invisible for anonymous
    {ok, Id} = m_rsc:update(Id, [{is_published, true}], z_acl:sudo(Context)),
    z:flush(),
    ?assertEqual(<<"Top secret!">>, m_rsc:p(Id, title, Context)). %% visible for anonymous

context() ->
    Context = z_context:new(testsandboxdb),
    start_modules(Context),
    Context.

start_modules(Context) ->
    ok = z_module_manager:activate_await(mod_content_groups, Context),
    ok = z_module_manager:activate_await(mod_acl_user_groups, Context).
