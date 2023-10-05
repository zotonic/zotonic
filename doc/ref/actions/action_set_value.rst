
.. include:: meta-set_value.rst


Set the value of a form field.

Example::

   <input type="text" id="x" name="xyz" value="">
   {% button text="fill" action={set_value target="x" value="etaoinstrdlu"} %}

Clicking on the button will set the value of the input element to the most interesting string ``etaoinstrdlu``.

This action can set the value of any input element, select or text area. It uses the jQuery ``val()`` method to set the value.

Optionally the argument ``trigger_event`` can be passed to trigger a change event::

   {% button text="fill" action={set_value target="x" value="..." trigger_event} %}

To trigger a custom event::

   {% button text="fill" action={set_value target="x" value="..." trigger_event="myevent"} %}

