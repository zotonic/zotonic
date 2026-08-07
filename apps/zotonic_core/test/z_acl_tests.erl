-module(z_acl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").

-export([acl_is_allowed/2]).

name_for_object_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    false = z_acl:is_allowed(update, ?ACL_ADMIN_USER_ID, Context).

anonymous_update_denied_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    z_notifier:observe(acl_is_allowed, {?MODULE, acl_is_allowed}, 1, Context),
    z_notifier:observe(acl_is_allowed_prop, {?MODULE, acl_is_allowed}, 1, Context),
    try
        lists:foreach(
            fun(Result) ->
                assert_anonymous_update_denied(
                    z_context:set(acl_is_allowed_result, Result, Context)
                )
            end,
            [undefined, false, true]
        )
    after
        z_notifier:detach(acl_is_allowed, Context),
        z_notifier:detach(acl_is_allowed_prop, Context)
    end.

assert_anonymous_update_denied(Context) ->
    lists:foreach(
        fun(Action) ->
            ?assertNot(z_acl:is_allowed(Action, ?ACL_ADMIN_USER_ID, Context)),
            ?assertNot(z_acl:maybe_allowed(Action, ?ACL_ADMIN_USER_ID, Context)),
            ?assertNot(z_acl:is_allowed_prop(Action, ?ACL_ADMIN_USER_ID, title, Context))
        end,
        [admin, insert, update, delete, link]
    ).

acl_is_allowed(#acl_is_allowed{}, Context) ->
    z_context:get(acl_is_allowed_result, Context);
acl_is_allowed(#acl_is_allowed_prop{}, Context) ->
    z_context:get(acl_is_allowed_result, Context).
