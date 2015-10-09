.. highlight:: django

.. index:: filter; escape
   single: mod_base; filter, escape
.. _filter-escape:

escape
======

* Module: :ref:`mod_base`

HTML escape a text. Escapes all reserved HTML characters in the value.
Escaped strings are safe to be displayed in a HTML page.  When you
echo a query string argument or path variable then you must escape the
value before displaying it on a HTML page.

The following characters are replaced:

+-------------+-------------+
|Character    |Replacement  |
+-------------+-------------+
|``>``        |``&gt;``     |
+-------------+-------------+
|``<``        |``&lt;``     |
+-------------+-------------+
|``"``        |``&quot;``   |
+-------------+-------------+
|``'``        |``&#039;``   |
+-------------+-------------+
|``&``        |``&amp;``    |
+-------------+-------------+

The escaping is only applied if the filter is not within an ``{% autoescape on %}`` block. 
If you always want escaping to be applied, use the :ref:`filter-force_escape`
filter.

For example::

  {{ value|escape }}

When the value is ``<hel&lo>`` then the output is ``&lt;hel&amp;lo&gt;``.

Note: this filter is not part of a module, it is built into ErlyDTL.

.. seealso:: :ref:`filter-force_escape`
