
Renders a template on the server and opens a dialog with the HTML output of the template.

Example::

   {% button text="cancel" action={dialog_open title="Select a name" template="_select_name.tpl" arg=100} %}

The title of this new dialog will be "Select a name", its contents are the output of rendering the template "_select_name.tpl". All arguments are handed as arguments to the template. In this example the template "_select_name.tpl" is rendered with the arguments "title", "template" and "arg".

.. seealso:: actions :ref:`action-dialog_close` and :ref:`action-dialog`.

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-dialog_open.rst>`_
