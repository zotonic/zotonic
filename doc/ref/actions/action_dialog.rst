
.. include:: meta-dialog.rst


Opens a dialog with a predefined HTML content and title.

Example::

   {% button action={dialog title="Wisdom" text="<p>The world is a pancake.</p>"} %}

This opens a dialog with the title "Wisdom".  The dialog is empty except for the text "The world is a pancake".

Normally, instead of this action, the action :ref:`action-dialog_open` is used. The action :ref:`action-dialog_open` shows a dialog that is rendered on the server.

========  ========  ==================================================
Argument  Required  Description                              
========  ========  ==================================================
title     required  Dialog header title
text      required  Dialog body text
width     optional  Dialog width in pixels
addclass  optional  classname will be appended to default dialog class
backdrop  optional  boolean (0, 1), or the string 'static' for a
                    modal dialog (does not close on backdrop click);
                    default 1
center    optional  boolean (0, 1) default 1; set to 0 to align the
                    dialog at the top
========  ========  ==================================================

.. seealso:: actions :ref:`action-dialog_open` and :ref:`action-dialog_close`.


