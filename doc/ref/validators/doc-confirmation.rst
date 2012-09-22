
Check if two inputs are the same.

Useful when a form requires double entry of a password or e-mail address to prevent typos.

Accepts an additional parameter `name` with the name of the other input field.

For example::

   <input type="password" id="password" name="password" value="" />
   <input type="password" id="password2" name="password2" value="" />
   {% validate id="password" type={confirmation match="password2"} %}

Arguments:

===============  ==========================================================  =======
Argument         Description                                                 Example
===============  ==========================================================  =======
match            The id of the input field that should have the same value.  match="field1"
failure_message  Message to be shown when the two fields are unequal.
                 Defaults to "Does not match."                               failure_message="Please retry."
===============  ==========================================================  =======



`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/validators/doc-confirmation.rst>`_
