.. _manual-validators:

Form validators
===============

Validators for HTML form fields.

Validators check if form fields have an acceptable value. They check
both client side and server side if the input fields are valid.
n

When an input field has been verified then it is available to Erlang
programs via the function `z_context:get_q_validated/2`.

When a client side input field does not validate on the server side
then the complete form submit is refused.

.. seealso:: listing of all :ref:`validators`, and the :ref:`scomp-validate` scomp.

