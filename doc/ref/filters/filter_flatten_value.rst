.. highlight:: django
.. include:: meta-flatten_value.rst

Flatten a list to a comma separated string.

Example::

    {{ [ "a", 100, "c" ]|flatten_value }}

Gives::

    a,100,c

As list of only integers in the range 32..255 is assumed to be a string.
