.. highlight:: django
.. include:: meta-linebreaksbr.rst

Translate ASCII newlines (``\n``) into HTML ``<br />`` elements.

The following string::
  
  {{ "foo\nbar"|linebreaksbr }}

will evaluate to ``foo<br />bar``.


.. seealso:: :ref:`filter-brlinebreaks`
