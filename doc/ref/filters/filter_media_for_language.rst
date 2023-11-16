
.. include:: meta-media_for_language.rst
.. seealso:: :ref:`filter-show_media`, :ref:`filter-embedded_media`, :ref:`filter-without_embedded_media`

Filter a list of media items by their ``medium_language`` property,
return the best matching with the current or given language. Only visible
media items are returned.

Let's assume two media resources:

 * 13925 with ``medium_language`` set to ``en``
 * 13926 with ``medium_language`` set to ``nl``

If the current language is ``nl`` then::

  {% print [13926, 13925]|media_for_language %}

Will show::

  [13926]

But::

  {% print [13926, 13925]|media_for_language:"en" %}

Will show::

  [13925]

The filter tries to select the *best* matching language for the requested
language. If there is no language requested, then the current request language is used.

If a language could not be found then the normal *fallback* language lookup
will apply, just as with text translations lookups.

For example, given the above two ids and a request language of ``nl`` then the following::

  {% print [13926, 13925]|media_for_language:"de" %}

Will print::

  [13926]

This is because there are no German translations. So the system fell back to the request
language(s) and selected the Dutch version.

This filter can be used to show all connected media that are in a certain
language::

  {% for media_id in m.edge.o[id].depiction %}
     {% media media_id %}
  {% endfor %}

