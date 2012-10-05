
.. index:: tag; media
.. _tag-media:

media
=====

Show embed, video or audio media.

The ``{% media %}`` tag is similar to the :ref:`tag-image` tag.  It accepts the same arguments but where ``{% image %}`` is guaranteed to give an still image, ``{% media %}`` can also generate embed, video or audio elements.

The ``{% media %}`` tag is not implemented in the core of Zotonic. It depends on modules implementing the tag which arguments and media formats are accepted.

An example of a module using ``{% media %}`` is :ref:`mod_video_embed` which enables the use of embed code from sites as youtube and vimeo.  Mod_video_embed will echo the embed code for a ``{% media %}`` tag and output a still image for an ``{% image %}`` tag.

.. seealso:: :ref:`tag-image`.
