
.. index:: tag; url
.. _tag-url:

url
===

Generate the url for a named dispatch rule.

The ``{% url %}`` tag generates an absolute url (i.e. an absolute path without hostname) for a named dispatch rule and arguments.  In this way it is possible to automatically change the generated urls when the dispatch rules are modified.

For example to generate the url to the admin for editing a :term:`page` (:term:`resource`), use::

   {% url admin_edit_rsc id=myid %}

Assuming myid is 42 then this will generate (on most Zotonic sites) the url “/admin/edit/42”.  The name “admin_edit_rsc” can be found in the :ref:`dispatch-mod_admin-dispatch` rules of :ref:`mod_admin`.  Which :ref:`dispatch rules <dispatch>` are available depends on which :ref:`ref-modules` are enabled.

When the dispatch rule named in the first argument is unknown then an empty string is returned.  There is no error message. This is to prevent breaking the web site when modules are enabled or disabled.

Arguments not named in the path of the dispatch rule are added to the query string of the returned url::

   {% url admin_edit_rsc id=42 foo="bar" %}

Returns the url “/admin/edit/42?foo=bar”.

Please note that the best way to generate the url of a page (resource) is to use::

   {{ m.rsc[myid].page_url }}

