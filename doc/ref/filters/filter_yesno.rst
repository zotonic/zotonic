.. highlight:: django
.. include:: meta-yesno.rst

Show a boolean value as a text.

Given a string mapping values for ``true``, ``false`` and (optionally)
``undefined``, returns one of those strings according to the value.

Non-empty values are converted to their boolean value first using
``z_convert:to_boolean/1``.

Example::

  {{ 1|yesno:"ja,nee" }}

Will output "ja"; this::

  {{ 0|yesno:"ja,nee" }}

Will output "nee".

`yesno` accepts these values:

+----------+---------------+----------+
|Value     |Argument       |Output    |
+==========+===============+==========+
|true      |“yeah,no,maybe”|“yeah”    |
+----------+---------------+----------+
|false     |“yeah,no,maybe”|“no”      |
+----------+---------------+----------+
|undefined |“yeah,no,maybe”|“maybe”   |
+----------+---------------+----------+
|undefined |“yeah,no”      |“no”      |
+----------+---------------+----------+

