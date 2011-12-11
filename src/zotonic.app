{application, zotonic,
 [{description, "zotonic"},
  {vsn, "0.7.3"},
  {modules, [
    zotonic,
    zotonic_app,
    zotonic_sup,
    zotonic_deps
  ]},
  {registered, []},
  {mod, {zotonic_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, mnesia]}]}.
