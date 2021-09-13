Zotonic Apps
============

Include this package in your project to fetch all core applications from Zotonic.

This package is releases and updated with every Zotonic release.

Usage
-----

In your rebar.config include zotonic in the deps:

    {deps, [
        zotonic_apps
    ]}.

Or use a specific version number:

    {deps, [
        {zotonic_apps, "1.0.0"}
    ]}.


Starting/stopping Zotonic
-------------------------

The `zotonic_apps` app will start zotonic_launcher, which will start all necessary other applications.
Ensure you have the zotonic application mentioned in your .app.src

    {application, my_zotonic_app,
     [{description, "Example project with Zotonic"},
      {vsn, git},
      {registered, []},
      {mod, {zotonic_app, []}},
      {applications, [
          kernel, stdlib, zotonic
      ]},
      {env, []},
      {modules,
       [
        my_zotonic_app,
        my_zotonic_sup
       ]}
     ]}.


TODO: ensure that all shell scripts are included in the packages and usable.
TODO: create an example umbrella project to use and clone as a template.
