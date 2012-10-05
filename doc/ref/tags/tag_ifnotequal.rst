
.. index:: tag; ifnotequal
.. _tag-ifnotequal:

ifnotequal
==========

Show a block when two values are not equal.

The ``{% ifnotequal %}`` tag tests if its two arguments are unequal.  If so then the contents of the ``{% ifnotequal %}`` block are output, otherwise the contents of the optional ``{% else %}`` block are output.

For example::

   {% ifnotequal value 5 %}
     Value is {{ value }} which is not 5.
   {% else %}
     Value is 5.
   {% endifnotequal %}

It is only possible to compare arguments that are variables (with optional filters) or constants.  Examples of constants are numbers, strings or lists.

.. seealso:: :ref:`tag-if` and :ref:`tag-ifequal`.
