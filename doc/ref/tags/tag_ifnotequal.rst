
.. index:: tag; ifnotequal
.. _tag-ifnotequal:

ifnotequal
==========

.. seealso:: :ref:`tag-if` and :ref:`tag-ifequal`.

Show something if two values are not equal.

The ``{% ifnotequal %}`` tag tests if its two arguments are unequal.  If so then the contents of the ``{% ifnotequal %}`` tag are output, otherwise the contents of the optional ``{% else %}`` tag are output.

For example::

   {% ifnotequal value 5 %}
     Value is {{ value }} which is not 5.
   {% else %}
     Value is 5.
   {% endifnotequal %}

It is only possible to compare arguments that are variables (with optional filters) or constants.  Examples of constants are numbers, strings or lists.
