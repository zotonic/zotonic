.. highlight:: django
.. index:: tag; if
.. _tag-if:

if
==

.. seealso:: :ref:`tag-ifequal` and :ref:`tag-ifnotequal`.

Show a block if the condition is true.

The ``{% if %}`` tag evaluates an expression and if the result is true (boolean true, number unequal to zero, non empty string or a non empty list) then the contents of the if-block are output.

.. note::
   Besides the ``{% elif %}`` tag we also support the alias ``{% elseif %}``.

If the if-test fails then the optional ``{% elif %}`` blocks are evaluated. If both the if-test and all elif-tests fail, then the ``{% else %}`` block contents are output.

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

   {% if new_moon or daytime %} Guess you can’t see the moon. {% endif %}

It is also possible to mix “and” and ”or” in one expression, so this is a valid::

   {% if full_moon or daytime and cloudy %}

The evaluation order is: ``and``, ``xor``, and then ``or``. Left to right associative.
So the above is equivalent to::

   {% if full_moon or (daytime and cloudy) %}

The ”not” operator can be used to negate a boolean value::

   {% if full_moon or daytime or not clearsky %}


if-with
-------

The ``if`` is often combined with the ``with`` tag. For example::

    {% with m.search[{latest cat=`news` pagelen=10}] as result %}
        {% if result %}
            <h3>{_ Latest news _}</h3>
            <ul>
              {% for id in result %}
                <li><a href="{{ id.page_url }}">{{ id.title }}</a></li>
              {% endfor %}
            </ul>
        {% endif %}
    {% endwith %}

To make this easier it is possible to combine the ``if`` and ``with`` tags in a single expression::

    {% if m.search[{latest cat=`news` pagelen=10}] as result %}
        <h3>{_ Latest news _}</h3>
        <ul>
          {% for id in result %}
            <li><a href="{{ id.page_url }}">{{ id.title }}</a></li>
          {% endfor %}
        </ul>
    {% endif %}

The ``as`` can also be used in the ``elif`` expressions::

    {% if expression1 as x %}
      ...
    {% elif expression2 as y %}
      ...
    {% else %}
      ...
    {% endif %}
