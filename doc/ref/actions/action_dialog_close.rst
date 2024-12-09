.. highlight:: django
.. include:: meta-dialog_close.rst
.. seealso:: actions :ref:`action-dialog_open` and :ref:`action-dialog`.

Closes a dialog. When there is no dialog open then nothing happens.

Example, closing the top-most dialog::

   {% button text="cancel" action={dialog_close} %}

This button closes any open dialog when clicked.

There can be many levels of dialogs open, they are designated by a *level*, the default
dialog opens at level 0. Higher levels are displayed above lower levels.

To close all open dialogs, pass level 0::

   {% button text="cancel" action={dialog_close level=0} %}
