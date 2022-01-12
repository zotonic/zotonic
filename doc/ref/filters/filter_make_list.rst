.. highlight:: django
.. include:: meta-make_list.rst

Forces the value to a list.

For example::

  {% print value|make_list %}

When value is the tuple ``{"a","b"}`` then the output is the list ``["a","b"]``.

This filter is especially useful for loops using the ``{% for %}`` tag::

  {% for v in value|make_list %}
    {{ v|escape }}
  {% endfor %}

