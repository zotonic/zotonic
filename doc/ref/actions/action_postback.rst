
.. include:: meta-postback.rst


This action sends a message to the event handler on the server.

Example::

   {% button title="Go" action={postback postback="go" action={growl text="sent message"}} %}

.. note::
   The :ref:`scomp-button` scomp can also take a postback argument directly.

After clicking the button the event `go` will be sent to the :term:`controller` module on the server and a :ref:`action-growl` message will be displayed.

The `event/2` function in the controller module will be called as::

   event({postback, go, TriggerId, TargetId}, Context)

This action can have the following arguments:

==============  =======================================================  =======
Argument        Description                                              Example
==============  =======================================================  =======
postback        The message that will be send to the server module.      postback={my_message arg="hello"}
delegate        The name of the Erlang module that will be called. 
                Defaults to the controller module generating the page.   delegate="my_module"
action          Any other actions that will be executed when the 
                postback is done.  This parameter can be repeated.       action={show target="wait"}
inject_args     If set to `true`, and postback is a tuple (as in the
                `my_message` example in this table), any values from 
                the args in the postback will replace the arg value
                in the postback argument. This is useful when the arg
                is coming from an outer action and not set explicitly
                in the template code (as is done in the example for
                illustration). The value of `some_arg` in the postback
                handler will be `123`.                                   {postback postback={my_event some_arg} inject_args some_arg=123}
qarg            Post the value of an input or select with the postback.  qarg="my-input-id"
                The value of the `qarg` argument is the id of the
                element to be posted.
                Multiple `qarg` arguments can be given.
                On the server the value will be available as a normal
                query argument using `z_context:get_q/2`
==============  =======================================================  =======


.. versionadded:: 0.9.0
   Added `inject_args` option.
