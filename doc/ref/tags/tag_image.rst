.. highlight:: django
.. index:: tag; image
.. _tag-image:

image
=====

Show a still image using a ``<img />`` element.

The ``{% image %}`` tag is used to generate an HTML ``<img />`` element for a
media resource. The image will be automatically resized to the desired size and
filters. For video, use the :ref:`tag-media` tag instead.

For example::

   {% image "cow.jpg" width=200 height=200 crop %}

This will generate an image tag for the image “cow.jpg” (in the files/archive
directory) of 200x200 pixels. The image will be resized and cropped to the
requested size.  The image tag will be something like (the checksum will vary
per sign key):

.. code-block:: html

   <img src="/image/cow.jpg%28200x200%29%28crop%29%28981E1AD8DA60381D00C094F0EE1311F5%29.jpg" width="200" height="200" />

The file argument can be one of the following:

* filename relative to the archive folder (“cow.jpg” is always present)

* resource id of a resource with attached file (mostly of the category “media”)

* property list of a resource’s medium record

Arguments
---------

width
^^^^^

The maximum with of the image. Example::

    width=200

height
^^^^^^

The maximum height of the image. Example::

    height=200

mediaclass
^^^^^^^^^^

The :ref:`media class <guide-media-classes>` of the image. Example::

    mediaclass="thumb"

background
^^^^^^^^^^

The background color for transparent image parts, specified as ImageMagick
colors. Example::

    background="white"

removebg
^^^^^^^^

Removes the image background. Accepts an optional fuzziness parameter (from 0
to 100). Examples::

    removebg
    removebg=50

blur
^^^^

Blur the image, making it less sharp. See ImageMagick blur for valid argument
values. Example::

    blur="20x8"

crop
^^^^

Crop the image. The resulting image will have the exact width and height as
described in the ``width`` and ``height`` arguments (see above).

The ``crop`` argument determines the cropping center. It either has the form
``+x+y`` (a set of coordinates in the image) or one of ``north``,
``north_east``, ``east``, ``south_east``, ``south``, ``south_west``, ``west``,
``north_west`` and ``center`` (the default). Examples::

    crop
    crop="south"
    crop="+100+100"

extent
^^^^^^

Add whitespace around the image until it fits the requested dimensions. Resize
the image so that it fits inside the width/height box, then extend the image
with a white background.

upscale
^^^^^^^

Forces the image to scale up to the requested dimensions.

flip
^^^^

Mirror left and right sides of the image.

flop
^^^^

Mirror the top and bottom of the image.

grey
^^^^

Make the image greyscale.

lossless
^^^^^^^^

Controls whether resized image should become JPG (``lossless=`false```) or
PNG images (``lossless=`true```). When set to ``auto``, PNG images will stay
PNG images when resized. This protects PNG graphics them from being encoded as
JPEGs and becoming blurry. Defaults to ``false``. Examples::

    lossless=`true`
    lossless=`auto`
    lossless=`false`

mono
^^^^

Make the image black and white.

quality
^^^^^^^

Set the quality of the resulting JPEG. An integer between 0 and 100, where 100
is best quality. The deafult quality depends on the resulting image size.
Smaller images will have a higher quality setting than bigger images. Note that
images smaller than 400x400 are sharpened before JPEG compression.

Example::

    quality=70

link
^^^^

Add a ``<a>`` tag around the generated ``<img>`` tag. The destination depends
on the value given.

Possible values:

* none; links to the image page itself
* an integer: to the page with that id
* any other value: assumed to be a URL.


alt
^^^

The text for the ``alt="..."`` attribute of the ``<img>``. Example::

    alt="A nice image"

class
^^^^^

The text for the ``class="..."`` attribute of the ``<img>``. Example::

    class="figure"

absolute_url
^^^^^^^^^^^^

Ensure that the generated URL contains the
:ref:`hostname and port <tag-url-absolute>`.

.. seealso::
    * :ref:`guide-media` developer guide.
    * :ref:`guide-media-classes` for some options that are only available in `mediaclass` files.
    * :ref:`tag-image_url` and :ref:`tag-media` tags.
