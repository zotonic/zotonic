
.. index:: tag; if
.. _tag-if:

if
==

Show a block if the condition is true.

The ``{% if %}`` tag evaluates a variable and if the result is true (boolean true, number unequal to zero, non empty string or a non empty list) then the contents of the if-block are output.  When the if-variable test fails then the optional ``{% elif %}`` blocks are evaluated. When the if and all optional elif variable tests fail, the optional ``{% else %}`` block content is output.

Example:

.. code-block:: django

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

The ”not” operator can be used to negate a boolean value::

   {% if full_moon or daytime or not clearsky %}

.. note::
   Besides the ``{% elif %}`` tag we also support the alias ``{% elseif %}``.


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

.. seealso:: :ref:`tag-ifequal` and :ref:`tag-ifnotequal`.
