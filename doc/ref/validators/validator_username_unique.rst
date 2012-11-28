.. highlight:: django
.. include:: meta-username_unique.rst

Check if an entered username is unique, by looking in the
:ref:`model-identity` table for the given username::

   <input type="text" id="username" name="username" value="" />
   {% validate id="username" type={username_unique} %}
       
Optionally, an `rsc_id` parameter can be given to the
validator to skip that particular id when doing the uniqueness
check. This is useful when you are displaying a form in which the user
is editing his own user name.

.. seealso:: :ref:`model-identity`, :ref:`validator-email_unique`
