
Check if an input has been filled in or checked.

For example when a title must be entered::

   <input type="text" id="title" name="title" value="" />
   {% validate id="title" type={presence} %}

Extra arguments:

===============  ========================================  =======
Argument         Description                               Example
===============  ========================================  =======
failure_message  Message to be shown when field is empty.
                 Defaults to "*"                           failure_message="Please enter."
===============  ========================================  =======

