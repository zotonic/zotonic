
.. include:: meta-mod_oembed.rst

Makes media :term:`resources <resource>` from embeddable URLs through
the `OEmbed <http://www.oembed.com/>`_ protocol.

When activated, in the "create media / page" dialog, an extra tab is
added which allows you to paste a media URL from services like
YouTube, Vimeo or any other service which supports OEmbed.

A saved media resource has a thumbnail image which is downloaded from
the OEmbed service and embedded in the resource. Furthermore, the
resource’s `medium` record has an ``oembed`` field which contains the
full JSON response of the request. The ``oembed`` field looks like
this::

  "oembed": {
    "type": "video",
    "version": "1.0",
    "provider_name": "Vimeo",
    "provider_url": "http://vimeo.com/",
    "title": "Heli Filming Showreel",
    "author_name": "Hot Knees Media",
    "author_url": "http://vimeo.com/hotknees",
    "is_plus": "1",
    "html": "<iframe src=\"http://player.vimeo.com/video/20898411\" width=\"640\" height=\"362\" ...",
    "width": 640,
    "height": 362,
    "duration": 106,
    "description": "description..",
    "thumbnail_url": "http://b.vimeocdn.com/ts/138/106/138106290_640.jpg",
    "thumbnail_width": 640,
    "thumbnail_height": 362,
    "video_id": 20898411
  }

So, to display the HTML of an OEmbedded medium, you would do the following in a template::

  {{ id.medium.html }}

The module also supports the use of the :ref:`tag-media` tag::
  
  {% media m.rsc[id].o.depiction.medium %}
  
Note however, that setting dimensions on this media tag is not
supported for OEmbed, as the embed width/height is always taken from
the provider.


Configuration options
---------------------

The following :ref:`model-config` options are supported:

``oembed.maxwidth``

  Requests the OEmbed service to return an HTML embed code with the
  requested maximum width. Defaults to 640.

``oembed.maxheight``

  Requests the OEmbed service to return an HTML embed code with the
  requested maximum height. Not set by default.
  
.. todo:: Extend documentation
