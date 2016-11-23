.. index:: tag; url
.. _tag-url:

url
===

Generate the URL for a named dispatch rule. In this way it is possible to
automatically change the generated URLs when the dispatch rules are modified.

For example to generate the URL to the admin for editing a :term:`page`, use::

   {% url admin_edit_rsc id=myid %}

Assuming myid is 42 then this will generate (on most Zotonic sites) the URL
“/admin/edit/42”.  The name “admin_edit_rsc” can be found in the
:ref:`dispatch-mod_admin-dispatch` rules of :ref:`mod_admin`.
Which :ref:`dispatch rules <guide-dispatch>` are available depends on
which :ref:`ref-modules` are enabled.

When the dispatch rule named in the first argument is unknown then an empty
string is returned.  There is no error message. This is to prevent breaking the
web site when modules are enabled or disabled.

Arguments not named in the path of the dispatch rule are added to the query
string of the returned URL::

   {% url admin_edit_rsc id=42 foo="bar" %}

Returns the URL “/admin/edit/42?foo=bar”.

Please note that the best way to generate the URL of a page (resource) is to use::

   {{ m.rsc[myid].page_url }}

.. _tag-url-absolute:

Generate absolute URLs
----------------------

By default, the ``{% url %}`` tag generates relative URLs. Add the argument
``absolute_url`` to generate absolute URLs that include the scheme and
hostname::

   {% url admin_edit_rsc id=42 foo="bar" absolute_url %}

will return a URL like “http://example.com/admin/edit/42?foo=bar”.

Finding out the current dispatch rule
-------------------------------------

The name of the current dispatch rule is always available in a
template under the name ``zotonic_dispatch``.

Check :ref:`ref-global-variables` for a full overview of variables
that are always available in the templates.
