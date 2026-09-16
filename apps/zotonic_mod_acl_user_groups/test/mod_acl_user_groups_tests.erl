%% @doc Tests for mod_acl_user_groups
%% @end
-module(mod_acl_user_groups_tests).
-moduledoc("
EUnit tests for ACL user-group authorization behavior.
").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    is_allowed_always_true/2
]).

-define(UG_TEST, acl_user_group_test).


tree_expand_test() ->
    [] = acl_user_groups_rules:tree_expand([]),
    [{1,[1]}, {2,[2]}] = acl_user_groups_rules:tree_expand([{1,[]}, {2,[]}]),
    [{1,[1]},{2,[2,3]},{3,[3]}] = acl_user_groups_rules:tree_expand([{1,[]}, {2,[{3,[]}]}]),
    ok.

person_can_edit_own_resource_test() ->
    ContextAnon = context(),
    ContextSudo = z_acl:sudo(ContextAnon),

    %% Person must be able to edit person category
    replace_managed(
        [
            {rsc, [
                {acl_user_group_id, ensure_test_group(ContextAnon)},
                {actions, [view, update]},
                {is_owner, true},
                {category_id, person}
            ]}
        ],
        ContextSudo),

    {ok, UserId1} = m_rsc:insert(#{ <<"category_id">> => person }, ContextSudo),
    {ok, UserId2} = m_rsc:insert(#{ <<"category_id">> => person, <<"creator_id">> => UserId1 }, ContextSudo),
    {ok, UserId3} = m_rsc:insert(#{ <<"category_id">> => person, <<"creator_id">> => self }, ContextSudo),

    m_edge:insert(UserId1, hasusergroup, ensure_test_group(ContextAnon), ContextSudo),
    m_edge:insert(UserId2, hasusergroup, ensure_test_group(ContextAnon), ContextSudo),
    m_edge:insert(UserId3, hasusergroup, ensure_test_group(ContextAnon), ContextSudo),

    ContextUser1 = z_acl:logon(UserId1, ContextAnon),
    ContextUser3 = z_acl:logon(UserId3, ContextAnon),

    % No access for anonymous
    ?assertEqual({error, eacces}, m_rsc:update(UserId1, [{title, <<"Test">>}], ContextAnon)),
    % Must be owner
    ?assertEqual({error, eacces}, m_rsc:update(UserId1, [{title, <<"Test">>}], ContextUser3)),
    % Must be creator
    ?assertEqual({error, eacces}, m_rsc:update(UserId2, [{title, <<"Test">>}], ContextUser3)),

    % User1 can update self, as user is self
    {ok, _} = m_rsc:update(UserId1, [{title, "Test"}], ContextUser1),
    % User1 can update user2, as user1 is creator (owner) if user2
    {ok, _} = m_rsc:update(UserId2, [{title, "Test"}], ContextUser1),
    % User3 can update self, as is owner of self (and is self)
    {ok, _} = m_rsc:update(UserId3, [{title, "Test"}], ContextUser3),

    m_rsc:delete(UserId1, ContextSudo),
    m_rsc:delete(UserId2, ContextSudo),
    m_rsc:delete(UserId3, ContextSudo),
    delete_managed(ContextSudo).


person_can_insert_text_in_default_content_group_only_test() ->
    ContextAnon = context(),
    ContextSudo = z_acl:sudo(ContextAnon),

    %% Person must be able to insert text into the default user group
    replace_managed(
        [
            % Allow insert of articles into default_content_group
            {rsc, [
                {acl_user_group_id, ensure_test_group(ContextSudo)},
                {content_group_id, default_content_group},
                {actions, [insert]},
                {is_owner, true},
                {category_id, article}
            ]},
            % Allow view of everything
            {rsc, [
                {acl_user_group_id, ensure_test_group(ContextSudo)},
                {actions, [view]}
            ]}
        ],
        ContextSudo),

    % Make a new user
    {ok, UserId} = m_rsc:insert(#{ <<"category_id">> => person }, ContextSudo),
    {ok, _} = m_edge:insert(UserId, hasusergroup, ensure_test_group(ContextSudo), ContextSudo),
    UserContext = z_acl:logon(UserId, ContextAnon),

    %% The user is able to insert a text into the default content group
    DefaultContentGroupId = m_rsc:p(default_content_group, id, ContextAnon),
    {ok, _TextId} = m_rsc:insert([{category, article}, {content_group_id, DefaultContentGroupId}], UserContext),

    %% But not in the system content group
    SystemContentGroupId = m_rsc:p(system_content_group, id, ContextAnon),
    ?assertEqual({error, eacces}, m_rsc:insert([{category, article}, {content_group_id, SystemContentGroupId}], UserContext)),

    m_rsc:delete(UserId, ContextSudo),
    delete_managed(ContextSudo),
    ok.


acl_is_allowed_accepts_rsc_name_object_test() ->
    ?assertEqual(false, acl_user_groups_checks:acl_is_allowed(#acl_is_allowed{action = insert, object = text}, context())).

%% @doc See https://github.com/zotonic/zotonic/issues/1306
acl_is_allowed_override_test() ->
    ContextAnon = context(),
    ContextSudo = z_acl:sudo(ContextAnon),

    {ok, UserId} = m_rsc:insert([{category_id, person}], ContextSudo),
    {ok, _} = m_edge:insert(UserId, hasusergroup, acl_user_group_anonymous, ContextSudo),
    ContextUser = z_acl:logon(UserId, ContextAnon),

    %% Priority (10) must be before mod_acl_user_group's acl_is_allowed observer.
    z_notifier:observe(acl_is_allowed, {?MODULE, is_allowed_always_true}, 10, ContextAnon),

    % Insert unpublished resource
    {ok, TextId} = m_rsc:insert([ {category_id, text} ], ContextUser),

    % Inserted into the default_content_group
    ?assertEqual(m_rsc:rid(default_content_group, ContextSudo), m_rsc:p_no_acl(TextId, <<"content_group_id">>, ContextSudo)),

    % Anon user can view but not update (short circuit in the ACL checks)
    ?assertEqual(true, z_acl:rsc_visible(TextId, ContextAnon)),
    ?assertEqual(false, z_acl:rsc_editable(TextId, ContextAnon)),

    % Authenticated user can view and update (due to our observer)
    ?assertEqual(true, z_acl:rsc_visible(TextId, ContextUser)),
    ?assertEqual(true, z_acl:rsc_editable(TextId, ContextUser)),

    z_notifier:detach(acl_is_allowed, ContextAnon),
    m_rsc:delete(UserId, ContextSudo),
    m_rsc:delete(TextId, ContextSudo).

publish_test() ->
    ContextAnon = context(),
    ContextSudo = z_acl:sudo(ContextAnon),

    %% Anonymous can view all published content
    replace_managed(
        [
            {rsc, [
                {acl_user_group_id, acl_user_group_anonymous},
                {actions, [view]}
            ]}
        ],
        ContextSudo),

    {ok, TextId} = m_rsc:insert([
            {is_published, false},
            {title, <<"Top secret!">>},
            {category, text}
        ], ContextSudo),

    ?assertEqual(<<"Top secret!">>, m_rsc:p(TextId, <<"title">>, ContextSudo)),

    %% invisible for anonymous
    ?assertEqual(false, z_acl:rsc_visible(TextId, ContextAnon)),
    ?assertEqual(undefined, m_rsc:p(TextId, <<"title">>, ContextAnon)),

    {ok, TextId} = m_rsc:update(TextId, [ {is_published, true} ], ContextSudo),

    %% visible for anonymous when published
    ?assertEqual(<<"Top secret!">>, m_rsc:p(TextId, <<"title">>, ContextAnon)),

    m_rsc:delete(TextId, ContextSudo),
    delete_managed(ContextSudo).

context() ->
    Context = z_context:new(zotonic_site_testsandbox),
    start_modules(Context),
    Context.

start_modules(Context) ->
    ok = z_module_manager:activate_await(mod_content_groups, Context),
    ok = z_module_manager:activate_await(mod_acl_user_groups, Context),
    ok = z_module_manager:upgrade_await(Context).

is_allowed_always_true(#acl_is_allowed{}, _Context) ->
    true.

replace_managed(Rules, Context) ->
    z_mqtt:subscribe(<<"model/acl_user_groups/event/acl-rules/publish-rebuild">>, z_acl:sudo(Context)),

    m_acl_rule:replace_managed(Rules, ?MODULE, z_acl:sudo(Context)),
    receive
        {mqtt_msg, _Msg} -> ok
    end,
    z_mqtt:unsubscribe(<<"model/acl_user_groups/event/acl-rules/publish-rebuild">>, z_acl:sudo(Context)).

delete_managed(Context) ->
    z_mqtt:subscribe(<<"model/acl_user_groups/event/acl-rules/publish-rebuild">>, z_acl:sudo(Context)),
    m_acl_rule:delete_managed(?MODULE, z_acl:sudo(Context)),
    receive
        {mqtt_msg, _Msg} -> ok
    end,
    z_mqtt:unsubscribe(<<"model/acl_user_groups/event/acl-rules/publish-rebuild">>, z_acl:sudo(Context)).

ensure_test_group(Context) ->
    case m_rsc:rid(?UG_TEST, Context) of
        undefined ->
            {ok, Id} = m_rsc:insert([
                    {name, ?UG_TEST},
                    {is_published, true},
                    {category_id, acl_user_group},
                    {title, <<"Test user group">>}
                ], z_acl:sudo(Context)),
            Id;
        Id ->
            Id
    end.
