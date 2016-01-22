.. include:: meta-validate.rst

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
|name           |Use this when the name of the input element is unequal to   |name="password_field"  |
|               |the id. The name is used by the server side code to         |                       |
|               |validate the received input. Defaults to the target         |                       |
|               |argument (which defaults to id).                            |                       |
+---------------+------------------------------------------------------------+-----------------------+
|valid_message  |Message to show when the field passes validation. Defaults  |valid_message="ok!"    |
|               |to the empty string.                                        |                       |
+---------------+------------------------------------------------------------+-----------------------+
|failure_message|Argument passed to the type argument. See individual        |                       |
|               |validators.                                                 |                       |
+---------------+------------------------------------------------------------+-----------------------+
|message_after  |The id of the element after which the failure message       |message_after=         |
|               |should be shown. Defaults to the id argument.               |"signup_tos_agree"     |
+---------------+------------------------------------------------------------+-----------------------+
|only_on_blur   |Normally validates on change, unles only_on_blur is set.    |only_on_blur           |
+---------------+------------------------------------------------------------+-----------------------+
|wait           |Time in msec to wait for validation after the last          |wait=100               |
|               |keystroke. Default: 0.                                      |                       |
+---------------+------------------------------------------------------------+-----------------------+
|only_on_submit |Whether the validation should be done when entering data or |only_on_submit         |
|               |only on submit of the form.  Set this to suppress validation|                       |
|               |when entering data.                                         |                       |
+---------------+------------------------------------------------------------+-----------------------+

.. seealso::

    * the list of :ref:`validators`
    * :ref:`Forms and validation <guide-validators>` in the Developer Guide
