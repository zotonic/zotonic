This directory contains modules that can be enabled or disabled on a site by site basis.
All modules are started by the supervisor in src/support/z_module_sup.erl

Any module is a directory with at least one similar named erlang module inside.
Besides the erlang module a module directory can contain dispatch rules, webmachine resources, templates and more.
