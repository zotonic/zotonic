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
