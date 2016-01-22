
.. include:: meta-custom.rst

Support for custom client-side (JavaScript-based) validators.

This call simply adds a ``z_add_validator()`` JavaScript call which
adds a custom validator based on the arguments given in the validator.

.. highlight:: django

Example::

    <input type="text" name="foobar" id="foobar" value="" />
    {% validate id="foobar" type={custom against="window.validate_foobar"} %}

.. highlight:: javascript

And then ``validate_foobar`` is defined as follows::

    function validate_foobar(value, args, isSubmit, submitTrigger)
    {
        // ... some logic to check the value
        return true or false;
    }

The ``args`` are available if the validation is added using the LiveValidation
JavaScript API.

``isSubmit`` is ``true`` if the validation is triggered by a form submit, if it
was triggered by change or focus events then it is ``false``.

``submitTrigger`` is the DOM tree node triggering the possible form submit.

Note that this validation does not do any server side validation. Because there
is no server side validation, the value of the ``input`` element is not 
available via ``z_context:get_q_validated/2`` but only via ``z_context:get_q/2``.

