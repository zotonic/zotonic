
Numerical input and range check.

Checks if the input is a number and within a certain range or equal to a fixed value.  At the moment only integer inputs are allowed.

Arguments are `is`, `minimum` and `maximum`.

For example, when the input must be `42`::

   <input type="text" id="number" name="number" value="" />
   {% validate id="number" type={numericality is=42} %}

And for a number within a certain range::

   <input type="text" id="percent" name="percent" value="" /> 
   {% validate id="percent" type={numericality minimum=0 maximum=100} %}

Arguments are:

+-----------------------+-----------------------------------------------------------+--------------------------------+
|Argument               |Description                                                |Example                         |
+=======================+===========================================================+================================+
|is                     |Tests for equality.                                        |is=42                           |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|minimum                |Minimum value.                                             |minimum=1                       |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|maximum                |Maximum value.                                             |maximum=100                     |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|not_a_number_message   |Message to show when the entered value is not a            |not_a_number_message="*"        |
|                       |number. Defaults to "Must be a number."                    |                                |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|not_an_integer_message |Message to show when the entered number is not an          |                                |
|                       |integer. Defaults to "Must be an integer."                 |                                |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|wrong_number_message   |Message to show when the entered number is unequal to the  |                                |
|                       |.is. argument. Defaults to "Must be .."                    |                                |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|too_low_message        |Message for when the entered number is less than the       |                                |
|                       |minimum allowed. Defaults to "Must not be less than .."    |                                |
+-----------------------+-----------------------------------------------------------+--------------------------------+
|too_high_message       |Message for when the entered number is greater than the    |                                |
|                       |maximum allowed. Defaults to "Must not be more than .."    |                                |
+-----------------------+-----------------------------------------------------------+--------------------------------+

