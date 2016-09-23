.. _guide-validators:

Forms and validation
====================

You `should validate all input data entered in forms`_. In Zotonic you create
forms by writing plain HTML. You can attach one or more validators to each input
element. Validators define acceptable values for the input data.

Let’s say you have a required input field ‘title’. To make sure some text is
entered in it, attach the :ref:`validator-presence` validator:

.. code-block:: django

    <input type="text" id="title" name="title" />
    {% validate id="title" type={presence} %}

The validated form field is available to Erlang code using the
``z_context:get_q_validated/2`` function::

    Title = z_context:get_q_validated(<<"title">>, Context).

Client- and server-side
-----------------------

Zotonic’s validators work both client- and server-side:

* JavaScript prevents the form from being submitted until the input data
  conforms to the validators.
* All validation is done on the server as well, which protects against users
  bypassing the validation checks in their browser.

Validators
----------

Zotonic comes with some commonly needed validators:

.. toctree::
   :maxdepth: 1
   :glob:

   ../ref/validators/validator_*

If you need to implement custom validation logic, use the
:ref:`validator-postback` validator. For JavaScript-only custom validation, use
the :ref:`validator-custom` validator.

.. seealso::

    * the :ref:`scomp-validate` tag
    * the :ref:`validator-postback` validator

.. _should validate all input data entered in forms: https://www.owasp.org/index.php/Data_Validation
