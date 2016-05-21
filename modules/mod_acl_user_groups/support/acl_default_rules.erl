%% @doc Default ACL rules

-module(acl_default_rules).

-export([
    get_default_rules/0
]).

%% @doc Get default ACL rules inserted by mod_acl_user_groups
-spec get_default_rules() -> list().
get_default_rules() ->
    [
        %% Anonymous can view everything
        {rsc, [
            {acl_user_group_id, acl_user_group_anonymous},
            {actions, [view]}
        ]}
    ] ++

    %% Managers can use management modules
    [use_module(acl_user_group_managers, Module) || Module <- get_modules(acl_user_group_managers)].

use_module(UserGroup, Module) ->
    {module, [
        {acl_user_group_id, UserGroup},
        {actions, [use]},
        {module, Module}
    ]}.

get_modules(acl_user_group_managers) ->
    [
        mod_admin,
        mod_admin_category,
        mod_admin_identity,
        mod_content_groups,
        mod_menu
    ].
