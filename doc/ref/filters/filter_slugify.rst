.. highlight:: django
.. include:: meta-slugify.rst

Converts a text into a slug.

Makes the value safe for use as a part in an url.  Mostly used for adding titles or descriptions to an url.

For example::

  {{ value|slugify }}

When value is “Nichts is unmöglich!” then the output will be “nichts-is-unmoglich”.

.. seealso:: :ref:`filter-stringify`
