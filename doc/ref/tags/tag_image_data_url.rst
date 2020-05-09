
.. index:: tag; image_data_url
.. _tag-image_data_url:

image_data_url
==============

Generate a ``data:`` url of a still image.

The ``{% image_data_url %}`` tag is used generate a data url with the image data.

``{% image_data_url %}`` accepts all parameters of the ``{% image %}`` tag but only outputs a data url and not the ``<img />`` element to display it.

Example::

    {% image_data_url "lib/images/trans.gif" %}

Generates::

    data:image/gif;base64,R0lGODlhAQABAJAAAAAAAAAAACH5BAEUAAAALAAAAAABAAEAAAICRAEAOw==

The ``image_data_url`` tag can be used in image tags or in css::

    <img src="{% image_data_url 'lib/images/trans.gif' %}">


.. seealso::
    * :ref:`guide-media` developer guide.
    * :ref:`guide-media-classes` for some options that are only available in `mediaclass` files.
    * :ref:`tag-image`, :ref:`tag-image_url` and :ref:`tag-media` tags.
