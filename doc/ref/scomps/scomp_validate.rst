
.. include:: meta-validate.rst

Add a validation to a form.

This tag connects validators to input elements of a form.

Validators check the input element with Javascript and prevent posting the form unless the validation is passed. All validations are also done on the server. This prevents people bypassing the validation checks. When the validation does not pass on the server side then the post will fail.

Validated form fields are available to Erlang code using the ``z_context:get_q_validated/2`` function.

For exampe, to check if two fields are equal::

   <input type="password" id="password" name="password" value="" />
   <input type="password" id="password2" name="password2" value="" />
   {% validate id="password" type={confirmation match="password2"} %}

The password field is now available to the Erlang code with::

   Password = z_context:get_q_validated("password", Context).

The validator tag accepts the following arguments:

+---------------+------------------------------------------------------------+-----------------------+
|Argument       |Description                                                 |Example                |
+===============+============================================================+=======================+
|id             |The id of the input element to be validated.                |id="password_field"    |
+---------------+------------------------------------------------------------+-----------------------+
|type           |The validator for the input element. Can be specified       |type={acceptance}      |
|               |multiple times to add multiple validators. The value depends|                       |
|               |on the validator choosen but is always a record             |                       |
|               |``{validatorname arg=value ...}``                           |                       |
+---------------+------------------------------------------------------------+-----------------------+
|trigger        |Id of the element that triggers the validation. Defaults to |trigger="title"        |
|               |the value of "id". The validator is triggered by a change of|                       |
|               |this. Almost never used.                                    |                       |
+---------------+------------------------------------------------------------+-----------------------+
|target         |Target of validator, defaults to the trigger. Almost never  |                       |
|               |used.                                                       |                       |
+---------------+------------------------------------------------------------+-----------------------+
|name           |Used when the id of the input element is unequal to the     |name="password_field"  |
|               |name. The name is used by the server side code to validate  |                       |
|               |the received input. Defaults to the target argument (which  |                       |
|               |defaults to id).                                            |                       |
+---------------+------------------------------------------------------------+-----------------------+
|valid_message  |Message to show when the field passes validation. Defaults  |valid_message="ok!"    |
|               |to the empty string.                                        |                       |
+---------------+------------------------------------------------------------+-----------------------+
|only_on_blur   |Normally validates on change, unles only_on_blur is set.    |only_on_blur           |
+---------------+------------------------------------------------------------+-----------------------+
|wait           |Time in msec to wait for validation after the last          |wait=100               |
|               |keystroke, default 0.                                       |                       |
+---------------+------------------------------------------------------------+-----------------------+
|only_on_submit |Whether the validation should be done when entering data or |only_on_submit         |
|               |only on submit of the form.  Set this to suppress validation|                       |
|               |when entering data.                                         |                       |
+---------------+------------------------------------------------------------+-----------------------+

.. seealso:: the list of :ref:`validators`.
