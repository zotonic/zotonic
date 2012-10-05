
.. index:: tag; image
.. _tag-image:

image
=====

Show a still image using a ``<img/>`` element.

The ``{% image %}`` tag is used to generate an HTML ``<img />`` element for a media resource. The image will be automatically resized to the desired size and filters.  See also the ``{% media %}`` tag for handling video.

For example::

   {% image "koe.jpg" width=200 height=200 crop %}

This will generate an image tag for the image “koe.jpg” (in the files/archive directory) of 200x200 pixels. The image will be resized and cropped to the requested size.  The image tag will be something like (the checksum will vary per :term:`sign key`)::

   <img src="/image/koe.jpg%28200x200%29%28crop%29%28981E1AD8DA60381D00C094F0EE1311F5%29.jpg" width="200" height="200" />

The file argument can be one of the following:

* filename relative to the archive folder (“koe.jpg” is always present)

* resource id of a resource with attached file (mostly of the category “media”)

* property list of a resource's medium record

The following arguments/filters can be specified:

+--------------------+------------------------------------------------------------+--------------------+
|Argument            |Description                                                 |Example             |
+--------------------+------------------------------------------------------------+--------------------+
|width               |The maximum width of the image.                             |width=200           |
+--------------------+------------------------------------------------------------+--------------------+
|height              |The maximum height of the image.                            |height=200          |
+--------------------+------------------------------------------------------------+--------------------+
|background          |The background color for transparent image parts. See       |background="white"  |
|                    |ImageMagick colors for how to specify the RGB color.        |                    |
+--------------------+------------------------------------------------------------+--------------------+
|blur                |Blur the image, making it less sharp. See ImageMagick blur  |blur="20x8"         |
|                    |for valid argument values.                                  |                    |
+--------------------+------------------------------------------------------------+--------------------+
|crop                |Crop the image, the resulting image will be exactly the     |crop="south"        |
|                    |width x height specified.  Which part of the image will be  |                    |
|                    |cropped depends on the value of the crop argument.  Default |                    |
|                    |is the center. Other options are: north, north_east, east,  |                    |
|                    |south_east, south, south_west, west and north_west.         |                    |
+--------------------+------------------------------------------------------------+--------------------+
|extent              |Resize the image so that it fits inside the width/height    |extent              |
|                    |box.  Then extent the image with a white background so that |                    |
|                    |it is centered and exactly the size of the box.             |                    |
+--------------------+------------------------------------------------------------+--------------------+
|flip                |Flip the image. Left and right will be mirrored.            |flip                |
+--------------------+------------------------------------------------------------+--------------------+
|flop                |Flop the image.  Top and bottom will be mirrored.           |flop                |
+--------------------+------------------------------------------------------------+--------------------+
|grey                |Make the image greyscale.                                   |grey                |
+--------------------+------------------------------------------------------------+--------------------+
|lossless            |Generate a PNG preview if the original file is a PNG, GIF or|lossless            |
|                    |PDF.                                                        |                    |
+--------------------+------------------------------------------------------------+--------------------+
|mono                |Make the image black and white.                             |mono                |
+--------------------+------------------------------------------------------------+--------------------+
|quality             |Set the quality of the resulting jpeg.  The quality is an   |quality=70          |
|                    |integer between 0 and 100, with 100 the best quality.  The  |                    |
|                    |default quality depends on the resulting image size.        |                    |
|                    |Smaller images will have a higher quality setting than      |                    |
|                    |bigger images.  Note that images smaller than 400x400 are   |                    |
|                    |sharpened before jpeg compression.                          |                    |
+--------------------+------------------------------------------------------------+--------------------+
|link                |Add a <a/> tag around the generated <img/> tag.  The        |link                |
|                    |destination depends on the value given.  Just “link” without|                    |
|                    |a value links to either the website property or the image   |                    |
|                    |page itself.  When an integer is given then the link is to  |                    |
|                    |the page with that id.  All other values are assumed to be  |                    |
|                    |an url.                                                     |                    |
+--------------------+------------------------------------------------------------+--------------------+
|alt                 |The text for the alt="..." attribute of the <img>.          |alt="A nice image"  |
+--------------------+------------------------------------------------------------+--------------------+
|class               |The text for the class="..."  attribute of the <img>.       |class="figure"      |
+--------------------+------------------------------------------------------------+--------------------+

.. seealso:: :ref:`tag-image_url` and :ref:`tag-media`.
