
.. include:: meta-mod_video.rst

Adds support for viewing and handling video medium items.

This module converts uploaded videos to h264 and adds a poster (preview) image of the movie.

.. note::
    mod_video uses the command-line utilities ``ffmpeg`` and ``ffprobe``. 
    For mod_video to function correctly they must be present in the search path of Zotonic.

Uploading & conversion
^^^^^^^^^^^^^^^^^^^^^^

The video module hooks into the media model to intercept any video upload.
If a video is uploaded the following steps are done:

 * The video is moved to the site’s ``files/video_queue/`` directory.
 * A video conversion task is added to the pivot task queue, this task will restart a video conversion in case of any problems.
 * A video conversion process is started, supervised by the video module.
 * The uploaded medium is replaced by a static ``lib/images/processing.png`` image.

Only a single video conversion process is allowed to run at any time. This to prevent overloading the server.

After the video is converted the resource’s medium record is replaced with the converted video.
The frame at 10 seconds (at 1 second for movies shorter than 30 seconds) is added as the preview image of the video.

If a video can’t be converted then the video is replaced with the error image, found in ``lib/images/broken.png``.

Viewing
^^^^^^^

The video module extends the ``{% media %}`` tag for viewing ``video/mp4`` videos.
It uses the template ``_video_viewer.tpl`` for viewing.
For the best viewing results it is best to add ``css/video.css`` to your included css files.

.. seealso:: :ref:`mod_video_embed`, :ref:`tag-media`

