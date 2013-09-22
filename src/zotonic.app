%% -*- mode: erlang -*-
{application, zotonic,
 [{description, "zotonic"},
  {vsn, "0.9.4"},
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
  {applications, [kernel, stdlib, crypto, mnesia,
                  bert, dh_date, eiconv, lager,
                  mimetypes, webzmachine, z_stdlib]}
 ]}.
