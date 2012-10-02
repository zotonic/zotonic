.. highlight:: django
.. include:: meta-brlinebreaks.rst

Translate HTML ``<br/>`` elements into ASCII newlines (``\n``).

The following string::
  
  {{ "foo<br/>bar"|brlinebreaks }}

will evaluate to ``foo\nbar``.

**Note:** Non-closing line breaks (``<br>``) are currently not converted.

.. seealso:: :ref:`filter-linebreaksbr`
