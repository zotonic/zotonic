.. _manual-modules:

The module system
=================

.. seealso:: listing of all :ref:`modules`.

Modules are the building blocks of Zotonic.

Examples of modules are the `/admin` site, `Atom feeds`, the `sitemap.xml`, video embed code handling and SEO optimization.  Modules also augment the functionality of other modules by adding extra :ref:`templates` and accompanying logic or adding handlers for internal Zotonic events. Good examples are the modules extending the :ref:`mod_admin`.

A module is a gen_server with accompanying templates, Webmachine resources (called :ref:`controllers`), :ref:`dispatch` and more, all contained in a single module directory tree.

The page :ref:`manual-module` has more information about the internals of a module.

**Looking for more modules?**

Check out the `Zotonic Module Index`_, an index with user-contributed modules which are not part of the core Zotonic distribution.

.. _Zotonic Module Index: http://modules.zotonic.com


Metadata
--------

Lorelay...


Module versioning
-----------------

Modules export a ``-module_schema()`` attribute which contains an
integer number, denoting the current module's version. On module
initialization, ``Module:manage_schema/2`` is called which handles
installation and upgrade of data. 

Minimal example::

  -module(mod_twitter).
  -mod_title("Twitter module").
  -mod_schema(3).  %% we are currently at revision 3
  
  -export([manage_schema/2]).
  .... more code here...
  
  manage_schema(install, Context) ->
    % .. code to install your stuff here, for instance:
    #datamodel{categories=
                 [
                  {tweet,
                   text,
                   [{title, <<"Tweet">>}]}
                 ]};
  
  manage_schema({upgrade, 2}, Context) ->
    %% code to upgrade from 1 to 2
    ok;
  
  manage_schema({upgrade, 3}, Context) ->
   %% code to upgrade from 2 to 3
   ok.
  
Note that the install function should always be kept up-to-date
according to the latest schema version. When you install a module for
the first time, no upgrade functions are called, but only the
``install`` clause.



