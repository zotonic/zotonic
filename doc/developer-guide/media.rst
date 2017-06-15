.. highlight:: django
.. _guide-media:

Media
=====

:term:`Resources <Resource>` can have media resources attached to them.
Resources and their media (images, video and audio) are connected through
‘depiction’ :term:`edges <edge>`. Additionally, resources can be *embedded* in
some HTML text, such as the resource’s body text.

Adding media
------------

Media is added to resources with the ‘Add media item’ button in the admin. You
can embed resources by using the admin’s rich text editor.

Programmatically, use the :ref:`model-media` model to insert the media resource:

.. code-block:: erlang

    {ok, MediaId} = m_media:insert_url("http://some-site/images/img.jpg", Context).

Or, with some properties:

.. code-block:: erlang

    Props = [
        {title, <<"This will be the title of the media resource">>},
        {category, audio},
        {is_published, true}
    ],
    {ok, MediaId} = m_media:insert_file("/path/to/audio.wav", Props, Context).

Then link the owning resource and the media resource:

.. code-block:: erlang

    m_edge:insert(Id, depiction, MediaId, Context).

In templates
------------

To render some resource’s (``id``) first depiction, use the :ref:`tag-image`
tag::

    {% image id %}

You can also add inline :ref:`parameters <tag-image-arguments>`:

    {% image id width=200 height=200 crop %}

To render embedded media, use the :ref:`filter-show_media` filter::

    {{ id.body|show_media }}

.. _guide-media-classes:

Media classes
^^^^^^^^^^^^^

Instead of inline image tag parameters, you can use media classes to define
image transformations. The advantage is that this image definition can then be
reused amongst templates.

Create a :file:`templates/mediaclass.config` file in your site directory:

.. code-block:: erlang

    [
        {"thumb", [
            {width, 200},
            {height, 200},
            crop
        ]}
    ].

This defines a media class called ‘thumb’, which can be used to display a
120x120 cropped square image. You then only need to refer to this media class in
your image tag::

    {% image id mediaclass="thumb" %}

The image URL will have a checksum embedded in it so that when the contents of
the media class is changed, all images which use that media class will be
regenerated to reflect the new media class.

Raw ImageMagick options
"""""""""""""""""""""""

Besides the normal image processing options, as described in :ref:`tag-image`,
it is possible to add literal ImageMagick convert commands to the mediaclass
definition.

For example::

    {magick, "-level 90%,100% +level-colors \\#FE7D18,\\#331575"}

(Note that you have to double any backslashes that were needed for the
``convert`` command line.)

This command is given *as-is* to the ImageMagick ``convert`` command, therefore it
is best to first try it with the command-line ``convert`` command to find the
correct options and command line escapes needed.

There are three variations: ``pre_magick``, ``magick``, and ``post_magick``.
The only difference is that the ``pre_magick`` is added before any other filter
argument, ``magick`` somewhere between, and ``post_magick`` after the last filter.

In this way it is possible to pre- or post-process an image before or after
resizing.

See http://www.imagemagick.org/Usage/ for examples of using ImageMagick from the
command line.
