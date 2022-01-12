.. highlight:: django
.. include:: meta-linebreaksbr.rst
.. seealso:: :ref:`filter-brlinebreaks`

Translate ASCII newlines (``\n``) into HTML ``<br />`` elements.

The following string::

  {{ "foo\nbar"|linebreaksbr }}

will evaluate to ``foo<br />bar``.
