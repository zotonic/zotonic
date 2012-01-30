{application, zotonic,
 [{description, "zotonic"},
  {vsn, "0.8-dev"},
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
