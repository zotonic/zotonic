-module(z_acl_tests).

-include_lib("eunit/include/eunit.hrl").

name_for_object_test() ->
    Context = z_context:new(zotonic_site_testsandbox),
    false = z_acl:is_allowed(update, administrator, Context).
