
Regular expression test.

Checks if an input element's value matches a regular expression.  There is an optional `negate` argument to validate only when the regular expression does not match.

For example, to test a dutch postal code::

   <input type="text" id="postcode" name="postcode" value="" />
   {% validate id="postcode" type={format pattern="^[0-9][0-9][0-9][0-9] +[A-Za-z][A-Z][a-z]$"} %}

Another example, no digits allowed::

   <input type="text" id="nodigit" name="nodigit" value="" />
   {% validate id="nodigit" type={format pattern="[0-9]" negate} %}

Accepts the following arguments:

===============  ======================================================  =======
Argument         Description                                             Example
===============  ======================================================  =======
pattern          The regular expression to match against.                pattern="[0-9][a-z]+"
negate           Specify negate when you want to accept values that do
                 not match the pattern.                                  negate
failure_message  Message to show when the input value does not match
                 the pattern (or does match the pattern when the negate
                 argument is given). Defaults to "Not valid."            failure_message="Invalid postcode"
===============  ======================================================  =======


`Edit <https://github.com/zotonic/zotonic/edit/master/doc/ref/validators/doc-format.rst>`_
