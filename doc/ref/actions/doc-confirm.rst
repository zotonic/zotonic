
Show a javascript confirm message and on confirmation triggers one or more actions and/or send a postback to the server.

Example::

   {% button action={confirm text="Format hard disk?" action={growl text="Better not"}} %}

Shows a javascript dialog with the question "Format hard disk?".  When this dialog is confirmed then the growl message "Better not" will appear. When the dialog is denied or canceled then nothing happens.

When there is a postback defined then the event handler for the postback will be called like::

   event({postback, Postback, TriggerId, TargetId}, Context).

Confirm accepts the following arguments:

========  ====================================  =======
Argument  Description                           Example
========  ====================================  =======
text      The text to be displayed.             text="Hola!"
action    One or more actions to be 
          executed on confirmation.
          This argument can be repeated.        action={alert text="plop"}
postback  Event to be sent back to the server.  postback="clicked_confirm"
delegate  Erlang module handling the postback. 
          Defaults to the controller 
          generating the page.                  delegate="my_event_module"
========  ====================================  =======

.. seealso:: actions :ref:`action-alert` and :ref:`action-growl`.

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-confirm.rst>`_
