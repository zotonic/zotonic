
.. index:: tag; raw
.. _tag-raw:

raw
===

Make a literal section which does not interpret tags.

The ``{% raw %}`` tag takes everything between the ``{% raw %}`` and ``{% endraw %}`` without interpretation. It is useful for surrounding javascript or pieces of code with (for example) ``{%`` in it.

Example::

   This echos: {{ a }}
   {% raw %}
   This does not echo {{ a }}
   {% endraw %}
   {{ a }}

Will output::

   This echos: hello
   This does not echo {{ a }}
   hello
