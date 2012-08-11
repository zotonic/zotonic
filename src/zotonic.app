{application, zotonic,
 [{description, "zotonic"},
  {vsn, "0.8.1"},
  {modules, [
    zotonic,
    zotonic_app,
    zotonic_sup,
    zotonic_deps
  ]},
  {registered, []},
  {mod, {zotonic_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, lager, mnesia]}]}.
