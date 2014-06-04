.. highlight:: django
.. include:: meta-truncate.rst

Truncate a text to a maximum length.

The text is truncated to the maximum length specified with the
argument. The text is always truncated at a word boundary. If the
truncation is not after punctuation then the unicode ellipsis `…` 
character is appended.

For example::

  {{ value|truncate:8 }}

If the value is ``hello world.`` then the output is ``hello…``.

Entities like ``&amp;`` are counted as a single character.

This filter is multibyte aware: Multi-byte UTF-8 characters are
assumed to be non-breaking characters.


Truncating character
....................

An optional second argument defines which text will be added if the text is truncated::

  {{ value|truncate:8:" (more)" }}

If the value is ``hello world.`` then the output is ``hello (more)``.


.. seealso:: :ref:`filter-truncate_html`
