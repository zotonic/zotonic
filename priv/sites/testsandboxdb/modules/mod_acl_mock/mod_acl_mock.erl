%% A mock acl module needed to minimize dependencies to run the testsandbox

-module(mod_acl_mock).
-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-mod_prio(1000).
-mod_provides([acl]).
