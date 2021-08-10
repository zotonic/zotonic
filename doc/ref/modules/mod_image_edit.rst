
.. include:: meta-mod_image_edit.rst

Non destructive edits of images.

This adds a *Edit image* button on the media panel in the admin.

Clicking that button opens an image editor where the following editing parameters can be set:

 * Rotation: in 90˚ increments, used to correct the orientation of an image
 * Crop: crop any or all sides of an image
 * Brightness: make an image more or less bright
 * Contrast: enhances or lowers the contrast
 * Roll: correct the rotation of an image (between 45˚ and -45˚)
 * Tilt: rotate an image in the Y direction, as if the viewer moves up or down
 * Pan: rotate an image in the X direction, as if the viewer moves left or right
 * Lossless flag: use PNG or GIF for the output image, used for clip art and logos
 * Crop center: mark a point that should stay in view when using automatic crops

All these operations are non-destructive. They are stored in the ``medium_edit_settings`` properties
and applied whenever an image is resized using the ``{% image %}`` tag.

The original image stays untouched. Downloads will download the original image.

If the original image should be used for a preview then pass the ``original`` argument to the ``{% image %}`` tag
options.

.. seealso:: :ref:`tag-image`, :ref:`tag-image-arguments`

