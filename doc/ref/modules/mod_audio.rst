
.. include:: meta-mod_audio.rst

Adds support for viewing and handling audio medium items.

This module parses audio files and extracts tags and an optional image from the audio file.

.. note::
    mod_audio uses the command-line utilities ``ffmpeg`` and ``ffprobe``.
    For mod_audio to function correctly they must be present in the search path of Zotonic.

Uploading
^^^^^^^^^

The audio module hooks into the media model to intercept any audio file upload.

If an audio file is uploaded then the file is parsed using ``ffprobe`` to fetch:

 * The bitrate
 * Audio duration
 * Tags, like artist, album
 * Album art image, extracted as png using ``ffmpeg``

The optional album art is used as the preview image. This image is visible when the
audio media item is rendered using a ``{% image ... %}`` tag.

Viewing
^^^^^^^

The audio module extends the ``{% media %}`` tag for viewing ``audio/*`` videos.

It uses the template ``_audio_viewer.tpl`` for viewing.

Editing
^^^^^^^

The following properties are extracted from the audio file, and can be edited in the
admin:

 * artist
 * album
 * album_artist
 * composer
 * genre
 * track
 * copyright
 * is_compilation (boolean flag)
 * org_pubdate, this is extracted form the ``creation_time`` tag

If there is no resource title given when creating the audio page then the title
from the tags is used. If that one is empty then the file’s basename is used.


.. seealso:: :ref:`mod_video`, :ref:`mod_video_embed`, :ref:`tag-media`

