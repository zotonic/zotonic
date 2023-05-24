.. highlight:: django
.. include:: meta-embedded_media.rst

.. seealso:: :ref:`filter-show_media`, :ref:`filter-without_embedded_media`

Fetch media ids that are embedded in the ``body``, ``body_extra``
and *text* blocks of your page.

This filter lets you loop over every image that is embedded in the
texts of the given page::

    {% for media_id in id|embedded_media %}
        {% media media_id width=315 extent %}
    {% endfor %}

Note that all translations of the texts are checked. This makes it possible
to add language-dependent media (with a text, or video) without them
showing up as extra depictions with other translations.

There is an optional (boolean) argument to only fetch media ids
from the ``body`` and ``body_extra`` properties::

    {% for media_id in id|embedded_media:0 %}
        {% media media_id width=315 extent %}
    {% endfor %}

You can also fetch all media ids embedded in a text::

    {% for media_id in id.body|embedded_media %}
        {% media media_id width=315 extent %}
    {% endfor %}
