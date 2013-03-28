
.. include:: meta-alert.rst


Show an alert dialog.

Example::

   {% button action={alert text="hello world"} %}

Shows an alert dialog with the text "hello world".

Alert accepts the following arguments:

=========  ================================  ================
Argument   Description                       Example
=========  ================================  ================
title      Title of the alert.               title="Alert"
text       The text to be displayed.         text="Hola!"
button     Text for the button. Defaults     button="Bummer"
           to "OK"
only_text  Set this to not show the "OK"     only_text
           button.
action     Action to be done when the user
           clicks on the OK button. There
           can be multiple actions.
=========  ================================  ================

The alert dialog is rendered using the ``_action_dialog_alert.tpl`` template.
Overrule this template to change the contents of the alert dialog.

.. seealso:: actions :ref:`action-growl` and :ref:`action-confirm`.
