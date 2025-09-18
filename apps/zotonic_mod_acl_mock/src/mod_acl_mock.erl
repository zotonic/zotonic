%% A mock acl module needed to minimize dependencies to run the testsandbox

-module(mod_acl_mock).
-moduledoc("
Mocking module for access control during tests.

**Do not use in production.**
").
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_prio(1000).
-mod_provides([acl]).
