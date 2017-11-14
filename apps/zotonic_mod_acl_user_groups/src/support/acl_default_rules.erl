%% @doc Default ACL rules

-module(acl_default_rules).

-export([
    get_default_rules/0
]).

%% @doc Get default ACL rules inserted by mod_acl_user_groups
-spec get_default_rules() -> list().
get_default_rules() ->
    lists:flatten([
        {rsc, [
            {acl_user_group_id, acl_user_group_anonymous},
            {actions, [view]},
            {content_group_id, default_content_group}
        ]},
        {rsc, [
            {acl_user_group_id, acl_user_group_anonymous},
            {actions, [view]},
            {content_group_id, system_content_group}
        ]},
        {rsc, [
            {acl_user_group_id, acl_user_group_editors},
            {actions, [insert, update, link, delete]},
            {content_group_id, default_content_group}
        ]},
        {rsc, [
            {acl_user_group_id, acl_user_group_managers},
            {actions, [insert, update, link, delete]},
            {content_group_id, default_content_group}
        ]},
        {rsc, [
            {acl_user_group_id, acl_user_group_managers},
            {actions, [insert, update, link, delete]},
            {content_group_id, system_content_group}
        ]},
        lists:map(
            fun(UserGroup) ->
                [
                    use_module(UserGroup, Module) || Module <- get_modules(UserGroup)
                ]
            end,
            [
                acl_user_group_anonymous,
                acl_user_group_members,
                acl_user_group_editors,
                acl_user_group_managers
            ])
    ]).

use_module(UserGroup, Module) ->
    {module, [
        {acl_user_group_id, UserGroup},
        {actions, [use]},
        {module, Module}
    ]}.

get_modules(acl_user_group_managers) ->
    [
          mod_admin
        , mod_acl_user_groups
        , mod_admin_category
        , mod_admin_config
        , mod_admin_identity
        , mod_admin_modules
        , mod_admin_statistics
        , mod_backup
        , mod_content_groups
        , mod_custom_redirect
        , mod_development
        , mod_email_status
        , mod_menu
        , mod_rest
        , mod_seo
        , mod_translation
    ];
get_modules(acl_user_group_editors) ->
    [
          mod_admin
        , mod_admin_statistics
        , mod_email_status
        , mod_menu
        , mod_rest
    ];
get_modules(acl_user_group_member) ->
    [
        mod_rest
    ];
get_modules(_) ->
    [
    ].
