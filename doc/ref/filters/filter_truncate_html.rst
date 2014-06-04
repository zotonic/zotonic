.. highlight:: django
.. include:: meta-truncate_html.rst

Truncate a HTML text to a maximum length.

The HTML text is truncated to the maximum length specified with the
argument. The text will be truncated at the maximum length, if any text
remains then the ellipsis character `…` is added and all open HTML tags
are closed.

For example::

  {{ value|truncate_html:8 }}

If the value is ``hello <b>world</b>`` then the output is ``hello <b>wo…</b>``.

Entities like "&amp;" are counted as a single character.

Self closing entities like ``<img/>`` and ``<br/>`` are not counted as characters.


Truncating character
....................

An optional second argument defines which text will be added if the text is truncated::

  {{ value|truncate_html:8:" (more)" }}

If the value is ``hello <b>world</b>`` then the output is ``hello <b>wo (more)</b>``.

.. seealso:: :ref:`filter-truncate`
