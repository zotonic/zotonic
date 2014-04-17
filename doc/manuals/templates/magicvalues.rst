.. _template-magicvalues:

Global template variables
-------------------------

The following properties are always available in a template.

zotonic_dispatch
  The name of the dispatch rule that was applied to render the current page.
q
  A dictionary containing the current request's query variables. For GET requests, these are the arguments passed from the query string (e.g. ``?foo=bar``); for POST requests, these are the values posted in the POST form. For more access to the raw request data, look at the :ref:`m_req` model.

now
  The local date and time in Erlang tuple notation, for instance ``{{2014,4,17},{13,50,2}}``.

m
  ``m`` is not really a value, but it's an indicator to trigger a lookup in one of Zotonic's :ref:`models`. For instance the :ref:`m_rsc` model is always exposed and can be used like this ``{{ m.rsc[123].title }}``.

Besides these variables, all key/value pairs that are set in the
``#context{}`` record (using ``z_context:set/2``) that was used to
render the current template are also exposed into the template's
global scope.
