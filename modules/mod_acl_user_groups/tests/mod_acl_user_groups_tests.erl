%% @doc Tests for mod_acl_user_groups
-module(mod_acl_user_groups_tests).

-include_lib("eunit/include/eunit.hrl").
-include("zotonic.hrl").

-export([is_allowed_always_true/2]).

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
    ?assertEqual({error, eacces}, m_rsc:update(Id, [{title, <<"Test">>}], Context)),

    UserContext = z_acl:logon(Id, Context),
    {ok, Id} = m_rsc:update(Id, [{title, "Test"}], UserContext).


person_can_insert_text_in_default_content_group_only_test() ->
    Context = context(),

    %% Person must be able to insert text into the default user group
    m_acl_rule:replace_managed(
        [
            {rsc, [
                {acl_user_group_id, acl_user_group_anonymous},
                {content_group_id, default_content_group},
                {actions, [insert]},
                {is_owner, true},
                {category_id, article}
            ]}
        ],
        ?MODULE,
        z_acl:sudo(Context)),

    %% Wait for ACL rules to be rebuilt
    timer:sleep(100),

    % Make a new user
    {ok, Id} = m_rsc:insert([{category, person}], z_acl:sudo(Context)),
    UserContext = z_acl:logon(Id, Context),

    %% The user is able to insert a text into the default content group
    DefaultContentGroupId = m_rsc:p(default_content_group, id, Context),
    {ok, _TextId} = m_rsc:insert([{category, article}, {content_group_id, DefaultContentGroupId}], UserContext),

    %% But not in the system content group
    SystemContentGroupId = m_rsc:p(system_content_group, id, Context),
    ?assertEqual({error, eacces}, m_rsc:insert([{category, article}, {content_group_id, SystemContentGroupId}], UserContext)),

    ok.



acl_is_allowed_accepts_rsc_name_object_test() ->
    ?assertEqual(false, acl_user_groups_checks:acl_is_allowed(#acl_is_allowed{action = insert, object = text}, context())).

%% @doc See https://github.com/zotonic/zotonic/issues/1306
acl_is_allowed_override_test() ->
    Context = context(),

    %% Priority (10) must be before mod_acl_user_group's acl_is_allowed observer.
    z_notifier:observe(acl_is_allowed, {?MODULE, is_allowed_always_true}, 10, Context),
    {ok, Id} = m_rsc:insert([{category_id, text}], Context),
    ?assertEqual(m_rsc:rid(default_content_group, Context), m_rsc:p(Id, content_group_id, Context)),
    z_notifier:detach(acl_is_allowed, {?MODULE, is_allowed_always_true}, Context).

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
    Context = z_context:new(testsandbox),
    start_modules(Context),
    Context.

start_modules(Context) ->
    ok = z_module_manager:activate_await(mod_content_groups, Context),
    ok = z_module_manager:activate_await(mod_acl_user_groups, Context),
    ok = z_module_manager:await_upgrade(Context).

is_allowed_always_true(#acl_is_allowed{}, _Context) ->
    true.
