.. highlight:: django
.. include:: meta-without_embedded_media.rst

Filter out media ids that are embedded in the ``body``, ``body_extra``
and *text* blocks of your page.

This filter lets you loop over every image that is not included in the
embedded ``body`` texts of the given page. This makes it easy to only
show images that have not been shown already::

  {% for media_id in m.rsc[id].media|without_embedded_media:id %}
      {% media media_id width=315 extent %}
  {% endfor %}

The only argument to the filter is the `id` of the page that you want to
consider for filtering from the body text.

There is an optional second argument to only consider media ids in the
``body`` and ``body_extra`` properties::

    {% for media_id in m.rsc[id].media|without_embedded_media:id:0 %}
        {% media media_id width=315 extent %}
    {% endfor %}

.. seealso:: :ref:`filter-show_media`, :ref:`filter-embedded_media`
