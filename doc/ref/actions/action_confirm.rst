
.. include:: meta-confirm.rst


Show a JavaScript confirm message and on confirmation triggers one or more actions and/or sends a postback to the server.

.. highlight:: django

Example::

   {% button action={confirm 
                     text="Format hard disk?" 
                     ok="Go Ahead!"
                     action={growl text="Better not"}
                     is_danger}
   %}

Shows a JavaScript dialog with the question "Format hard disk?".  If this dialog is confirmed then the growl message "Better not" will appear. If the dialog is denied or canceled then nothing happens.

.. highlight:: erlang

If there is a postback defined then the event handler for the postback will be called like::

   event(#postback{message=Message, trigger=TriggerId, target=TargetId}, Context).

Confirm accepts the following arguments:

=============  ====================================  =====================================
Argument       Description                           Example
=============  ====================================  =====================================
text           The text to be displayed.             text=_"The answer to life and the rest?"
title          Title above the alert, defaults to
               ``_"Confirm"``                        title=_"Rescue the world, with an answer"
ok             The text of the ok button, defaults
               to ``_"OK"``                          text="42"
cancel         The text of the cancel button, 
               defaults to ``_"Cancel"``             text="No, thanks for the fish"
text_template  Template used to render the text,
               all action arguments are passed to
               the template.                         text_template="_fancy_confirm.tpl"
action         One or more actions to be 
               executed on confirmation.
               This argument can be repeated.        action={alert text="you said ok"}
on_cancel      One or more actions to be
               executed on cancelation               on_cancel={alert text="you said cancel"}
postback       Event to be sent back to the server  
               if the ok button is clicked.          postback="clicked_confirm"
delegate       Erlang module handling the postback. 
               Defaults to the controller 
               generating the page.                  delegate="my_event_module"
is_danger      If the 'ok' button should be flagged  is_danger
               as dangerous.
=============  ====================================  =====================================

.. seealso:: actions :ref:`action-alert` and :ref:`action-growl`.
