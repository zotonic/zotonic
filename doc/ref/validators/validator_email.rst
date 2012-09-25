
.. include:: meta-email.rst


Check if the content of the input field is an e-mail address.

For example::

   <input type="text" id="email" name="email" value="" />
   {% validate id="email" type={email} %}

Arguments:

===============  =================================================  =======
Argument         Description                                        Example
===============  =================================================  =======
failure_message  Message to show when the entered value is not
                 an e-mail address. Defaults to "Incorrect E-mail"  failure_message="Please enter your e-mail address."
===============  =================================================  =======

