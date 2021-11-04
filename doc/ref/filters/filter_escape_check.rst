.. highlight:: django

.. index:: filter; escape_check
   single: mod_base; filter, escape_check
.. _filter-escape_check:

escape_check
============

* Module: :ref:`mod_base`

Ensures thant an HTML escaped value is properly escaped.

Checks for all reserved HTML characters if they are properly escaped.

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

If you always want escaping to be applied, use the :ref:`filter-force_escape`
filter.

For example::

  {{ value|escape_check }}

When the value is ``<hel&amp;lo>`` then the output is ``&lt;hel&amp;lo&gt;``.

.. seealso:: :ref:`filter-force_escape`, :ref:`filter-escape`
