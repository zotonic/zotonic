.. highlight:: django
.. index:: tag; image
.. _tag-image:

image
=====

.. seealso:: :ref:`guide-media` developer guide.

.. seealso:: :ref:`guide-media-classes` for some options that are only available in `mediaclass` files.

.. seealso:: :ref:`tag-image_url`, :ref:`tag-image_data_url` and :ref:`tag-media` tags.

Show a still image using an ``<img>`` element. The image will be automatically
resized to the desired size and filters. For video, use the :ref:`tag-media` tag
instead.

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

* property list of a resource’s medium record.

So, to render a resource’s depiction::

    {% image id width=200 height=200 %}

Please note that even if you supply no arguments, the image will be processed to
be scaled, based on the :ref:`image-quality` argument’s default value.

.. _tag-image-arguments:

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

nowh
^^^^

Do not generate the width and height attributes. Per default
the image width and height are calculated and added to the
generated ``img`` tag. This option prevents the addition of
width and height attributes.

mediaclass
^^^^^^^^^^

The :ref:`media class <guide-media-classes>` of the image. Example::

    mediaclass="thumb"

The mediaclass is added as a CSS class to the generated image, prefixed with
``mediaclass-``.  For example::

    {% img mediaclass="large" %}

Might become::

    <img src="/image/2021/3/29/pic.jpg%28mediaclass-large.e911845ed2e692bab8ebceb676409a04066cf43b%29.jpg"
         decoding="async" alt="" class="mediaclass-large" width="2459" height="800">

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

Optionally a background color can be given as well:

    removebg="black,50"

blur
^^^^

Blur the image, making it less sharp. See ImageMagick blur for valid argument
values. Example::

    blur="20x8"


rotate3d
^^^^^^^^

Rotate the image in three dimensions: roll, tilt and pan. The size of the original image and canvas
is maintained. This results in clipping at the edges.

rotate3d is useful for straigthening photos of flag objects that are taken under an angle.

The rotate3d accepts three arguments::

    rotate3d=[ 1, 2, -3 ]

This rotates the image:

 * Roll 1 degree clockwise around the Z axis (like css rotateZ).
 * Tilt 2 degrees clockwise around the X axis (like css rotateX)
 * Pan 3 degrees counter clockwise around the Y axis (like css rotateY)

The center of the rotation is at the center of the image.

rotate3d, rotate and cropp are applied before other operations.

rotate
^^^^^^

Rotate the image by a multiple of 90 degrees. The image size is adjusted according to the
rotation::

    rotate=90

This will rotate the image 90 degrees clockwise. Use a negative number to rotate counter clockwise.

Acceptable values are: ``0``, ``90``, ``180``, ``270``, ``-90``, ``-180``, and ``-270``

rotate3d, rotate and cropp are applied before other operations.

cropp
^^^^^

Crop percentages from the sides of an image. The image is cropped by a percentage of the
width and/or height. The crop's argument is a list of numbers, in the order: left, right, top, bottom::

    cropp=[ 10, 15, 20, 30 ]

The example above crops 10% from the left side, 15% from the right, 20% from the top, and
30% from the bottom. The resulting image will be 65% of the original width and 50% of the
original height.

rotate3d, rotate and cropp are applied before other operations.

crop
^^^^

Crop the image. The resulting image will have the exact width and height as
described in the ``width`` and ``height`` arguments (see above).

The ``crop`` argument determines the cropping center. It either has the form
``+x+y`` (a set of coordinates in the image) or one of ``north``,
``north_east``, ``east``, ``south_east``, ``south``, ``south_west``, ``west``,
``north_west`` and ``center`` (the default). To define the cropping in your
template::

    crop="south"
    crop="+100+100"
    crop=[100, 100]

The cropping center can also be determined by editors on the media item’s admin
page (using :ref:`mod_image_edit`). Without any argument, the image will be cropped
around the user-defined cropping center::

    crop

The coordinate of the cropping center is relative to the original image, before rotate and
cropp operations.

If :ref:`mod_media_exif` and :ref:`mod_image_edit` are enabled then the focal point information
of the image is taken as the cropping center for automatic cropping.


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

brightness
^^^^^^^^^^

Change the brightness of an image. A percentage in the range of -100 .. 100(%).
Negative values darken the image, positive brighten the image. This applies
a lineair multiplier to the input image, similar to the css brighten filter.
Defaults to 0, no change.

contrast
^^^^^^^^

Change the contrast of an image. A percentage in the range of -100 .. 100(%).
Negative values decrease the contrast of the image, positive values increase
the contrast. A value of -100 results in a gray image.
Defaults to 0, no change.

lossless
^^^^^^^^

Controls whether resized image should become JPEG (``lossless=false``) or
PNG/GIF images (``lossless=true``). When set to ``auto``, PNG and GIF images will stay
PNG/GIF images after resizing.

This protects PNG/GIF clip art and logos from being encoded as JPEGs and becoming blurry.

Defaults to ``false``. Examples::

    lossless=true
    lossless=`auto`
    lossless=false

mono
^^^^

Make the image black and white.

.. _image-quality:

quality
^^^^^^^

Set the quality of the resulting JPEG. An integer between 0 and 100, where 100
is best quality. The default quality is inversely proportional to the output
image resolution: higher-resolution images still look good even with a limited
quality. Note that images smaller than 400x400 are sharpened before JPEG
compression.

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
