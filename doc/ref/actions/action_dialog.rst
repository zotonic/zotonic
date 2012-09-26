
.. include:: meta-dialog.rst


Opens a dialog with a predefined HTML content and title.

Example::

   {% button action={dialog title="Wisdom" text="<p>The world is a pancake.</p>"} %}

This opens a dialog with the title "Wisdom".  The dialog is empty except for the text "The world is a pancake".

Normally, instead of this action, the action :ref:`action-dialog_open` is used. The action :ref:`action-dialog_open` shows a dialog that is rendered on the server.

.. seealso:: actions :ref:`action-dialog_open` and :ref:`action-dialog_close`.


