
.. include:: meta-set_class.rst


Set the value of a form field.

Example::

   <input type="text" id="x" name="xyz" value="" />
   {% button title="fill" action={set_value target="x" value="etaoinstrdlu"} %}

Clicking on the button will set the value of the input element to the most interesting string `etaoinstrdlu`.

This action can set the value of any input element, select or text area. It uses the jQuery `val()` method to set the value.
