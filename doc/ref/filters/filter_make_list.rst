.. highlight:: django
.. include:: meta-make_list.rst

Forces the value to a list.

This is especially useful for loops using the ``{% for %}`` tag.

For example::

  {{ value|make_list }}

When value is the tuple ``{"a","b"}`` then the output is the list ``["a","b"]``.

When the input is a :term:`model`, this filter forces the model to
call its ``m_to_list/2`` function, which is used to iterate over an
entire model, e.g.::

  {% print m.req|make_list %}

This will show something like::

  [{version,{1,1}},
   {peer,"127.0.0.1"},

Instead of printing just ``{m, req, undefined}`` when the filter was
not applied.
   
