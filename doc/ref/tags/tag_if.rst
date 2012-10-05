
.. index:: tag; if
.. _tag-if:

if
==

Show a block if the condition is true.

The ``{% if %}`` tag evaluates a variable and if the result is true (boolean true, number unequal to zero, non empty string or a non empty list) then the contents of the if-block are output.  When the if-variable test fails then the optional ``{% else %}`` block content are output.

Example::

   {% if person_list %}
     There are {{ person_list|length }} persons.
   {% else %}
     There are no persons.
   {% endif %}

When the person_list is not empty then the number of persons is displayed, otherwise the ``{% else %}`` block is displayed.

An ``{% if %}`` tag can have an “and” or “or” expression as argument::

   {% if person_list and show_persons and full_moon %}
     There are persons that we can show during full moon.
   {% endif %}

Or for example::

   {% if new_moon or daytime %} Guess you can't see the moon. {% endif %}

It is also possible to mix “and” and ”or” in one expression, so this is a valid::

   {% if full_moon or daytime and cloudy %}

The ”not” operator can be used to negate a boolean value::

   {% if full_moon or daytime or not clearsky %}
