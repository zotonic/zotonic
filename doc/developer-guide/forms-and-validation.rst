.. _guide-validators:

Forms and validation
====================

Validators for HTML form fields.

Validators check if form fields have an acceptable value. They check
*both client side and server side* if the input fields are valid.

Validators check the input element with JavaScript and prevent posting the form
unless the validation is passed. All validations are also done on the server.
This prevents people bypassing the validation checks in their browser. When the
validation does not pass on the server side then the post will fail.

Validated form fields are available to Erlang code using the ``z_context:get_q_validated/2`` function.

When an input field has been verified then it is available to Erlang
programs via the function ``z_context:get_q_validated/2``.

To check if two fields are equal:

.. code-block:: django

    <input type="password" id="password" name="password" value="" />
    <input type="password" id="password2" name="password2" value="" />
    {% validate id="password" type={confirmation match="password2"} %}

The password field is now available to the Erlang code with::

   Password = z_context:get_q_validated("password", Context).

.. seealso::

    * listing of :ref:`all validators <validators>`
    * the :ref:`scomp-validate` tag.

