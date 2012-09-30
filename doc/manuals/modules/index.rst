.. _manual-modules:

The module system
=================

Modules are the building blocks of Zotonic.

Examples of modules are the `/admin` site, `Atom feeds`, the
`sitemap.xml`, video embed code handling and SEO optimization.
Modules also augment the functionality of other modules by adding
extra :ref:`templates` and accompanying logic or adding handlers for
internal Zotonic events. Good examples are the modules extending the
:ref:`mod_admin`.

A module is a directory containing the module's Erlang code,
:ref:`controllers <manual-templates>`, :ref:`controllers
<manual-controllers>`, :ref:`dispatch rules <manual-dispatch>` and
more, all contained in a single module directory tree.

.. toctree::
   :maxdepth: 2

   structure
   gen_server
   dependencies
   versioning

.. seealso:: listing of all :ref:`ref-modules`.


**Looking for more modules?**

Check out the `Zotonic Module Index <http://modules.zotonic.com>`_:,
an index with additional user-contributed modules which are not part
of the core Zotonic distribution.
