
.. index:: tag; filter
.. _tag-filter:

filter
======

Filter the contents of a block through variable filters.

Filters can also be piped through to each other.

Example::

   {% filter escape | lower %}
   The text will be lowered and escaped. So you can use <, > and & without any problems.
   {% endfilter %}

.. versionadded:: 0.8
