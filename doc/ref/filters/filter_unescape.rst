.. highlight:: django
.. include:: meta-unescape.rst

Removes HTML escaping from a text.

Expands the entities added by the :ref:`filter-escape` filter or
:ref:`filter-force_escape` filter.  This is useful when you want to
display a field from the database in a text-only format medium.

For example::

  Title: {{ m.rsc[id].title|unescape }}

Be careful that you only use this filter when you are absolutely sure
that the output is not used in HTML or XML.

.. seealso:: :ref:`filter-escape`, :ref:`filter-force_escape`
