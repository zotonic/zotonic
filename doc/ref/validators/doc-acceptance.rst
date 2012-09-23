
Check if an input value evaluates to true.

Can be used in combination with a check box that must be checked on submit.

For example::

   <input type="checkbox" id="accept" name="accept" value="1" />
   {% validate id="accept" type={acceptance} %}

Arguments:

===============  ===========================================  =======
Argument         Description                                  Example
===============  ===========================================  =======
failure_message  Message to be shown when the input is true.
                 Defaults to "Must be accepted."              failure_message="Please agree to our TOS."
===============  ===========================================  =======

