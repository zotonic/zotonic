%% -*- mode: erlang -*-
{application, zotonic,
 [{description, "zotonic"},
  {vsn, "0.9.3"},
  {modules,
   [
    zotonic,
    zotonic_app,
    zotonic_sup,
    zotonic_deps,
    z
   ]},
  {registered, []},
  {mod, {zotonic_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, lager, webzmachine, mnesia]}]}.
