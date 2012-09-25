
.. include:: meta-postback.rst


Server side validation.

Performs a server side validation of an input value.

For example::

   <input type="text" id="username" name="username" value="" />
   {% validate id="username" type={postback delegate="mod_useradmin"} %}

The validation is either done by a function or by an event handler.

The module of the function is defined by the argument `delegate`.  The name of the event is defined by the argument `event`.

Example of a validator function::

   validate(postback, Id, Value, Args, Context) ->
   {{ok, Value}, Context}.

Example of an event handler function, which will be called by `z_notifier:first/2`::

   validate_event({validate_username, {postback, Id, Value, Args}}, Context) ->
   {{ok, Value}, Context}.

Both should have the following return type::

   {{ok,AcceptedValue}, NewContext} | {{error,Id,Error}, NewContext}
   Error -> invalid | novalue | {script, Script} | novalidator | string()

Arguments:

+---------+------------------------------------------------------------------------------------------+---------------------------+
|Argument |Description                                                                               |Example                    |
+=========+==========================================================================================+===========================+
|delegate |The module to handle the validation.  Must implement and export the function validate/5.  |delegate="myvalidator"     |
+---------+------------------------------------------------------------------------------------------+---------------------------+
|event    |Name of an event to be broadcast with z_notifier:first/2 for handling the validation.     |event=">validate_username" |
+---------+------------------------------------------------------------------------------------------+---------------------------+

