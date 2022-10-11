.. highlight:: django
.. include:: meta-round_significant.rst

Round a number value to a number of significant digits.
The significance defaults to two digits.

Example::

  {{ 1256|round_significant }}
  {{ 1256|round_significant:1 }}
  {{ 1256|round_significant:3 }}

Results in::

  1300
  1000
  1260

Floating point values are also rounded to a number of digits. For example
if ``n`` is set to ``1256.78`` then::

  {{ n|round_significant }}
  {{ n|round_significant:3 }}
  {{ n|round_significant:5 }}

Results in::

  1300.0
  1260.0
  1256.8

Input values that are not a number are converted to a floating point value. If the
conversion fails then ``undefined`` is returned.
