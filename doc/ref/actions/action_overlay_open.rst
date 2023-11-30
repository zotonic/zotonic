.. highlight:: django
.. include:: meta-overlay_open.rst
.. seealso:: actions :ref:`action-overlay_close`, :ref:`action-dialog_open` and :ref:`action-dialog`.

Renders a template on the server and opens a full screen overlay with the HTML output of the template.

Example::

   {% button text="show story" action={overlay_open template="_story.tpl" id=1234} %}

This opens an overlay over the current content. The template ``_story.tpl`` will be rendered with
the argument ``id`` (and possibly any other arguments). The rendered html will then be shown
inside the overlay.

The overlay template is a ``div`` with the class ``modal-overlay``. Extra classes can be added using
the ``class`` argument::

   {% wire action={overlay_open template="_splash.tpl" class="splash"} %}

The overlay action has the following arguments:

=========  ====================================  =======================
Argument   Description                           Example
=========  ====================================  =======================
template   Template to render in the overlay     template="_overlay.tpl"
class      Extra CSS class(es) to add to the     class="myclass other"
           overlay diff.
level      Nesting of the overlay. Non negative  level="top"
           integer, higher numbered levels are
           displayed above lower levels.
           Special level ``"top"`` to force
           display on top, above all dialogs
           and other overlays.
=========  ====================================  =======================

All (extra) arguments are passed to the rendered template.
