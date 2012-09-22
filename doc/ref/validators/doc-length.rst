
Check the length of a text input.

Test if the length of the input's value is more than a minimum and/or less than a maximum length.

Arguments are `minimum` and `maximum`. You can give either or both arguments.

For example, a summary that is neither too long nor too short::

   <textarea id="summary" name="summary"></textarea>
   {% validate id="summary" type={length minimum=20 maximum=200} %}

Arguments:

====================  ==========================================================  =======
Argument              Description                                                 Example
====================  ==========================================================  =======
is                    Use when the value must be a specific length.               is=4
minimum               The minimum length of the value.                            minimum=4
maximum               The maximum length of the value.                            maximum=10
wrong_length_message  Message for when the length is unequal to the value of the
                      "is" argument. Defaults to "Must be . characters long." 
too_short_message     Message for when there are not enough characters entered.
                      Defaults to "Must not be less than . characters long." 
too_long_message      Message for when there are too many characters entered.
                      Defaults to "Must not be more than . characters long." 
====================  ==========================================================  =======


`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/validators/doc-length.rst>`_
