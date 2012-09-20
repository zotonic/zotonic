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


Versioning
----------

Sing-a-long-song...

