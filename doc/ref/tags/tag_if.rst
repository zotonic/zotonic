
.. index:: tag; if
.. _tag-if:

if
==

Show a block if the condition is true.

The ``{% if %}`` tag evaluates a variable and if the result is true (boolean true, number unequal to zero, non empty string or a non empty list) then the contents of the if-block are output.  When the if-variable test fails then the optional ``{% elif %}`` blocks are evaluated. When the if and all optional elif variable tests fail, the optional ``{% else %}`` block content is output.

Example::

   {% if genre == "pop" %}
     Popular music.
   {% elif genre == "classical" %}
     Classical music.
   {% elif genre == "jazz" %}
     Jazz
   {% else %}
     The genre isn't pop, classical or jazz.
   {% endif %}

An ``{% if %}`` and ``{% elif %}`` tag can have an “and” or “or” expression as argument::

   {% if person_list and show_persons and full_moon %}
     There are persons that we can show during full moon.
   {% endif %}

Or for example::

   {% if new_moon or daytime %} Guess you can't see the moon. {% endif %}

It is also possible to mix “and” and ”or” in one expression, so this is a valid::

   {% if full_moon or daytime and cloudy %}

The ”not” operator can be used to negate a boolean value::

   {% if full_moon or daytime or not clearsky %}

.. note:: 
   Besides the ``{% elif %}`` tag we also support the alias ``{% elseif %}``.

.. seealso:: :ref:`tag-ifequal` and :ref:`tag-ifnotequal`.
