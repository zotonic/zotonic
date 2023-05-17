.. highlight:: django
.. include:: meta-dialog.rst
.. seealso:: actions :ref:`action-dialog_open`, :ref:`action-dialog_close` and :ref:`action-overlay_open`.

Opens a dialog with a predefined HTML content and title.

Example::

   {% button action={dialog title="Wisdom" text="<p>The world is a pancake.</p>"} %}

This opens a dialog with the title "Wisdom".  The dialog is empty except for the text "The world is a pancake".

Normally, instead of this action, the action :ref:`action-dialog_open` is used. The action :ref:`action-dialog_open` shows a dialog that is rendered on the server.

There can be many levels of dialogs open, they are designated by a *level*, the default
dialog opens at level 0. Higher levels are displayed above lower levels. There is a special level ``"top"``
which ensures that a dialog is always opened above any other open dialog.

========  ========  ==================================================
Argument  Required  Description
========  ========  ==================================================
title     required  Dialog header title
text      required  Dialog body text
width     optional  Dialog width in pixels. Use ``"large"`` for a
                    wide dialog and ``"small"`` for a small dialog.
addclass  optional  classname will be appended to default dialog class
backdrop  optional  boolean (0, 1), or the string ``"static"`` for a
                    modal dialog (does not close on backdrop click);
                    default 1
center    optional  boolean (0, 1) default 1; set to 0 to align the
                    dialog at the top
keyboard  optional  boolean (true, false) default: true; if true,
                    closes when escape keys is pressed
level     optional  Nesting of the dialog. Non negative integer, higher
                    numbered levels are displayed above lower levels.
                    Special level ``"top"`` to force display on top.
========  ========  ==================================================

