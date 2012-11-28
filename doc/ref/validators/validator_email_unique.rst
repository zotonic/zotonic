.. highlight:: django
.. include:: meta-email_unique.rst

Check if an entered e-mail address is unique, by looking in the
:ref:`model-identity` table for the `email` key::

   <input type="text" id="email" name="email" value="" />
   {% validate id="email" type={email} type={email_unique} %}
       
Optionally, an `rsc_id` parameter can be given to the
validator to skip that particular id when doing the uniqueness
check. This is useful when you are displaying a form in which the user
is editing his own email address.

.. seealso:: :ref:`model-identity`, :ref:`validator-username_unique`
