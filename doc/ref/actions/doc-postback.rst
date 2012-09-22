
This action sends a message to the event handler on the server.

Example::

   {% button title="Go" action={postback postback="go" action={growl text="sent message"}} %}

.. note::
   The :ref:`scomp-button` scomp can also take a postback argument directly.

After clicking the button the event `go` will be sent to the :term:`controller` module on the server and a :ref:`action-growl` message will be displayed.

The `event/2` function in the controller module will be called as::

   event({postback, go, TriggerId, TargetId}, Context)

This action can have the following arguments:

========  ======================================================  =======
Argument  Description                                             Example
========  ======================================================  =======
postback  The message that will be send to the server module.     postback={my_message arg="hello"}
delegate  The name of the Erlang module that will be called. 
          Defaults to the controller module generating the page.  delegate="my_module"
action    Any other actions that will be executed when the 
          postback is done.  This parameter can be repeated.      action={show id="wait"}
========  ======================================================  =======

`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/actions/doc-postback.rst>`_
