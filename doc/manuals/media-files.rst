.. _manual-media:

Media file handling
===================


.. _manual-media-classes:

Media classes
-------------

So-called `media classes` define a way to give a specific image
transformation a name and re-use it amongst templates.

The :ref:`tag-image` tag has a `mediaclass` attribute which expects
the name of a mediaclass that is defined somewhere.

Zotonic uses the template lookup mechanism to look for files called
``mediaclass.config`` under the `templates/` directories of your
modules/sites. An example ``mediaclass.config`` file is the following::
  
  [
   {"thumb",
    [
     {width, 120},
     {height, 120},
     crop
    ]}
  ].
  
This defines a media class called `"thumb"`, which can be used to
display a 120x120 cropped square image. This media class can then be
used in the image tag as follows::

  {% image id mediaclass="thumb" %}

The image URL will have a checksum embedded in it so that when the
contents of the media class is changed, all images which use that
media class will be regenerated to reflect the new media class.


ImageMagick conversion options
..............................

Besides the normal image processing options, as described in :ref:`tag-image`, it is
possible to add literal ImageMagick convert commands to the mediaclass definition.

For example::

	{magick, "-level 90%,100% +level-colors \\#FE7D18,\\#331575"}

(Note that you have to double any backslashes that were needed for the `convert` command line.)

This command is given *as-is* to the ImageMagick `convert` command, therefore it is best to
first try it with the command-line `convert` command to find the correct options and command line
escapes needed.

There are three variations: `pre_magick`, `magick`, and `post_magick`.
The only difference is that the `pre_magick` is added before any other filter argument, `magick`
somewhere between, and `post_magick` after the last filter.

In this way it is possible to pre- or post-process an image before or after resizing.

See http://www.imagemagick.org/Usage/ for examples of using ImageMagick from the command line.


User-agent specific images
..........................

Since the ``mediaclass.config`` file is resolved using the template
selection mechanism, it is subject to the same selection rules that
normal templates fall under.

The consequence is that you can have multiple ``mediaclass.config``
files, for instance one in `desktop/`, one in `phone/`. The media
classes defined in those subdirectories can have the same names. This
way you can make thumbnail sizes smaller for phones, or serve
higher-quality JPEG file for desktop browsers.

See :ref:`manual-lookup-system-ua` for the details on the user-agent
selection mechanism.


.. seealso:: :ref:`tag-image`, :ref:`tag-media`, :ref:`manual-lookup-system-ua`
