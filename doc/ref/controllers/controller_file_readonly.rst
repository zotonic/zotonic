
.. include:: meta-file_readonly.rst

Serve a file or image.

This controller is used to serve files and images. It is able to
manipulate an image according to the parameters supplied.

Image manipulation parameters are signed to prevent random image
manipulations on the request of visitors, which might result in a
denial of service due to processing- or disk space limitations.

This controller serves all files with a very long client side caching
time. It also handles if-modifies-since checks. It also compresses
served files with gzip when the user-agent supports it.

Example dispatch rule::

  {image, ["image", '*'], resource_file_readonly, [{is_media_preview, true}]}

Resource_file_readonly has the following dispatch options:

+---------------------+-------------------------------------+------------------------+
|Option               |Description                          |Example                 |
+---------------------+-------------------------------------+------------------------+
|root                 |List of root directories where files |{root, [lib]}           |
|                     |are located. Use 'lib' for the       |                        |
|                     |library files. This defaults to the  |                        |
|                     |site's “files/archive” directory,    |                        |
|                     |unless “is_meda_preview” is set then |                        |
|                     |it defaults to the sites’s           |                        |
|                     |“files/preview” directory.           |                        |
+---------------------+-------------------------------------+------------------------+
|media_path           |The path for media when              |{media_path,            |
|                     |“is_media_preview” is set. Defaults  |"/var/media/archive"}   |
|                     |to the site’s “files/archive”        |                        |
|                     |directory.                           |                        |
+---------------------+-------------------------------------+------------------------+
|path                 |Default file to be served.  Used for |{path,"misc/robots.txt"}|
|                     |files like "robots.txt" and          |                        |
|                     |"favicon.ico".                       |{path, id}              |
|                     |                                     |                        |
|                     |When set to the atom 'id' then there |                        |
|                     |must be an 'id' argument in the      |                        |
|                     |dispatch list. The file attached to  |                        |
|                     |this resource is then served.        |                        |
+---------------------+-------------------------------------+------------------------+
|content_disposition  |If the file should be viewed in the  |{content_disposition,   |
|                     |browser or downloaded. Possible      |inline}                 |
|                     |values are ``inline`` and            |                        |
|                     |``attachment``. Defaults to the      |                        |
|                     |browser’s defaults by not setting the|                        |
|                     |“Content-Disposition” response       |                        |
|                     |header.                              |                        |
+---------------------+-------------------------------------+------------------------+
|is_media_preview     |Set to true to allow recognition and |{is_media_preview, true}|
|                     |handling of image manipulation       |                        |
|                     |parameters. See the :ref:`tag-image` |                        |
|                     |tag for their format. Defaults to    |                        |
|                     |false.                               |                        |
+---------------------+-------------------------------------+------------------------+
|use_cache            |Use server side caching of           |{use_cache, true}       |
|                     |files. Especially useful when        |                        |
|                     |gzip-compressing files. Not so useful|                        |
|                     |when a proxy cache like Varnish is   |                        |
|                     |used. Defaults to false.             |                        |
+---------------------+-------------------------------------+------------------------+

controller_file_readonly does not handles any query arguments other than the file path.

