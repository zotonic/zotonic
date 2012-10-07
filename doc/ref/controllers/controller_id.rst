
.. include:: meta-id.rst

Handle different content representations of a page.

Redirects to different representations of a page, depending on the
requested content type. The redirect is done using a “303 See Other”
status. When the requested page does not exist then a “404 Not Found”
is returned.

When no content types are requested then ``text/html`` is selected.

This controller is also used for a page’s short url representation.

Example dispatch rule (from mod_base)::

  {id, ["id", id], controller_id, []}

This controller does not have any dispatch options.

This controller handles the following query argument:

+---------------------+-------------------------------------+------------------------+
|Option               |Description                          |Example URL             |
+---------------------+-------------------------------------+------------------------+
|id                   |Id of the requested :term:`resource`.|/id/1234                |
+---------------------+-------------------------------------+------------------------+

The list of provided content types is collected with a `foldr`
notification (see :ref:`manual-notification`) of the type
``content_types_dispatch``. Modules should add their provided content
types in front of the accumulator. The added entries are tuples:
``{MimeType, DispatchRuleName}``.

Example of adding a content type handler adding a text/plain handler
with the dispatch rule ``rsc_text``::

  observe_content_types_provided(content_types_dispatch, Acc, Context) ->
      [{"text/plain", rsc_text} | Acc].
