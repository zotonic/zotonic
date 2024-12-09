.. highlight:: django
.. include:: meta-truncatechars.rst
.. seealso:: :ref:`filter-truncate`, :ref:`filter-truncate_html`

Truncate a text to a maximum length in characters.

The text is truncated to the maximum length specified with the
argument. The text is truncated at a characters boundary, use
:ref:`filter-truncate` to truncate at a word boundary. If the
truncation is not after punctuation then the unicode ellipsis `…`
character is appended.

For example::

  {{ value|truncatechars:8 }}

If the value is ``hello world.`` then the output is ``hello wo…``.

Entities like ``&amp;`` are counted as a single character.


Truncating character
....................

An optional second argument defines which text will be added if the text is truncated::

  {{ value|truncatechars:8:" (more)" }}

If the value is ``hello world.`` then the output is ``hello wo (more)``.
