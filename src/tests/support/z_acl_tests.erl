-module(z_acl_tests).
-author("david").

-include_lib("eunit/include/eunit.hrl").

name_for_object_test() ->
    Context = z_context:new(testsandboxdb),
    false = z_acl:is_allowed(update, administrator, Context).
