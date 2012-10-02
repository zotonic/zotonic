.. highlight:: django
.. include:: meta-pprint.rst

Pretty print a zotonic value in a template.

Pretty printing a zotonic value in a template is handy during development. It outputs the value of an erlang variable in Html. 

Usage::

  {{ value | pprint }}

This output is similar to the :ref:`tag-print` tag, only are the values of the
pprint filter not wrapped in ``<pre>`` tag.

.. seealso:: :ref:`tag-print`
