.. _template-magicvalues:

Global template variables
-------------------------

The following properties are always available in a template.

zotonic_dispatch
  The name of the dispatch rule that was applied to render the current page.

z_language
  The currently selected language. This an atom, for example: `en`.

q
  A dictionary containing the current request's query variables. For GET requests, these are the arguments passed from the query string (e.g. ``?foo=bar``); for POST requests, these are the values posted in the POST form. For more access to the raw request data, look at the :ref:`model-req` model.

now
  The local date and time in Erlang tuple notation, for instance ``{{2014,4,17},{13,50,2}}``.

m
  ``m`` is not really a value, but it's an indicator to trigger a lookup in one of Zotonic's :ref:`models`. For instance the :ref:`model-rsc` model is always exposed and can be used like this ``{{ m.rsc[123].title }}``.

Besides these variables, all key/value pairs that are set in the
``#context{}`` record (using ``z_context:set/2``) that was used to
render the current template are also exposed into the template's
global scope.


Magic query arguments
---------------------

Besides the query arguments supplied in the request and from dispatch-rule bindings there are some additional query variables available:

zotonic_dispatch
   The name of the current dispatch rule, see also above.

zotonic_dispatch_path
   A list containing the request path used for matching the dispatch rule. The path is split on ``/`` and after an optional rewrite. This means that the list doesn’t contain the language prefix. For example, the path ``/en/foo/bar?a=b`` will give the list ``["foo", "bar"]``.

z_language
   The current language, matched from the request path.

z_trigger_id
   Only available in postback contexts. The id of the html element triggering a postback.

z_target_id
   Only available in postback contexts. The id of the html element that is the target of a postback.

z_delegate
   Only available in postback contexts. The name of the Erlang module handling the postback event.
