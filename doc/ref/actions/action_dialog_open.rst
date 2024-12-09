.. highlight:: django
.. include:: meta-dialog_open.rst
.. seealso:: actions :ref:`action-dialog_close`, :ref:`action-dialog` and :ref:`action-overlay_open`.

Renders a template on the server and opens a dialog with the HTML output of the template.

Example::

   {% button text="cancel" action={dialog_open title="Select a name" template="_select_name.tpl" arg=100} %}

The title of this new dialog will be "Select a name", its contents are the output of rendering the template "_select_name.tpl". All arguments are handed as arguments to the template. In this example the template "_select_name.tpl" is rendered with the arguments "title", "template" and "arg".

There can be many levels of dialogs open, they are designated by a *level*, the default
dialog opens at level 0. Higher levels are displayed above lower levels. There is a special level ``"top"``
which ensures that a dialog is always opened above any other open dialog.

Example, opening a dialog above the default dialog::

   {% button text="ok" action={dialog_open title="Confirm" template="_confirm.tpl" level=1} %}

Example, opening a dialog above any open dialog::

   {% button text="ok" action={dialog_open title="Confirm" template="_confirm.tpl" level="top"} %}
