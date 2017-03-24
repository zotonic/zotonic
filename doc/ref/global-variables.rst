.. _ref-global-variables:

Global variables
================

These variables are always available for
:ref:`rendering in templates <template-variables>`.

zotonic_version
----------------

The version of Zotonic, for example ``"1.0-dev"``.

zotonic_dispatch
----------------

An atom with the name of the dispatch rule that was applied to render the current page.

zotonic_dispatch_path
---------------------

A list containing the request path used as initial input for the dispatcher.
The path is split on ``/`` and after an optional rewrite. This means that the
list doesn’t contain the language prefix. For example, the path
``/en/foo/bar?a=b`` will give the list ``["foo", "bar"]``.

zotonic_dispatch_path_rewrite
-----------------------------

Same as zotonic_dispatch_path, but set to the path after an optional internal
request rewrite inside the dispatcher. For example if a resource has its
`page_path` set to ``/foo`` and the requested path is ``/en/foo`` then the
``zotonic_dispatch_path`` will be set to ``["foo"]`` and the
``zotonic_dispatch_path_rewrite`` could be set to something like
``["page", "1234", "foo-slug"]``.

z_language
----------

The currently selected language. This an atom, for example: ``en``.

q
-

A dictionary containing the current request's query variables. For GET requests, these are the arguments passed from the query string (e.g. ``?foo=bar``); for POST requests, these are the values posted in the POST form. For more access to the raw request data, look at the :ref:`model-req` model.

now
---

The local date and time in Erlang tuple notation, for instance ``{{2014,4,17},{13,50,2}}``.

m
-

``m`` is not really a value, but it's an indicator to trigger a lookup in one of Zotonic's :ref:`models`. For instance the :ref:`model-rsc` model is always exposed and can be used like this ``{{ m.rsc[123].title }}``.

