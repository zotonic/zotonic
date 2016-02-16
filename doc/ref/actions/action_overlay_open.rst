
.. include:: meta-overlay_open.rst

Renders a template on the server and opens a full screen overlay with the HTML output of the template.

Example::

   {% button text="show story" action={overlay_open template="_story.tpl" id=1234} %}

This opens an overlay over the current content. The template ``_story.tpl`` will be rendered with
the argument ``id`` (and possibly any other arguments). The rendered html will then be shown
inside the overlay.

.. seealso:: actions :ref:`action-overlay_close`, :ref:`action-dialog_open` and :ref:`action-dialog`.
