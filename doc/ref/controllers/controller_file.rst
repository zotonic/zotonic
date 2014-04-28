
.. include:: meta-file.rst

Serve an uploaded-, resized- or library file.

This controller is used to serve files and images. It is able to
manipulate an image according to the parameters supplied.

Image manipulation parameters are signed to prevent random image
manipulations on the request of visitors, which might result in a
denial of service due to processing- or disk space limitations.

This controller serves all files with a very long client side caching
time and handles if-modified-since checks. Text files are served with gzip
compression if the user-agent supports it.

Multiple files can be served in a single request; the controller concatenates them
into a single file. See the :ref:`tag-lib` tag for more information. 
The creators of the files have to ensure that they can be properly concatenated.

Dispatch rules and options
^^^^^^^^^^^^^^^^^^^^^^^^^^

Example dispatch rules::

    {image, ["image", '*'], controller_file, []},
    {lib, ["lib", '*'], controller_file, [{root, [lib]}]}

controller_file has the following dispatch options:

+---------------------+-------------------------------------+------------------------+
|Option               |Description                          |Example                 |
+---------------------+-------------------------------------+------------------------+
|root                 |List of root directories where files |{root, [lib]}           |
|                     |are located. Use 'lib' for the       |                        |
|                     |library files. This defaults to the  |                        |
|                     |site’s “files/archive” directory.    |                        |
+---------------------+-------------------------------------+------------------------+
|path                 |Default file to be served.  Used for |{path,"misc/robots.txt"}|
|                     |files like "robots.txt" and          |                        |
|                     |"favicon.ico".                       |                        |
+---------------------+-------------------------------------+------------------------+
|content_disposition  |If the file should be viewed in the  |{content_disposition,   |
|                     |browser or downloaded. Possible      |inline}                 |
|                     |values are ``inline`` and            |                        |
|                     |``attachment``. Defaults to the      |                        |
|                     |browser’s defaults by not setting the|                        |
|                     |“Content-Disposition” response       |                        |
|                     |header.                              |                        |
+---------------------+-------------------------------------+------------------------+
|acl                  |Extra authorization checks to be     |See `ACL options`_.     |
|                     |performed.                           |                        |
+---------------------+-------------------------------------+------------------------+
|max_age              |Max age, used for Cache and Expires. |{max_age,3600}          |
|                     |Value is an integer, number of secs. |                        |
+---------------------+-------------------------------------+------------------------+

.. include:: acl_options.rst

More about the search root
^^^^^^^^^^^^^^^^^^^^^^^^^^

The search root can be a list with one or more of the following:

 * The atom `lib` for finding library files in the *lib* directory of modules.
 * The atom `template` for finding files in the *template* directory of modules.
 * A directory name (binary or string). This directory name must be absolute or relative
   to the *files* directory of the site.
 * A tuple `{module, ModuleName}` to refer to a module. The module must implement the
   functions `file_exists/2` and `file_forbidden/2`.

CSS and JavaScript templates
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If a file with a `lib` or `template` root is not found, then the same filename with the 
addition of `.tpl` is checked. For example `styles.css.tpl`.
If found then the template will be rendered against an empty site context.
This means that, with the current implementation, the template will not receive the
current language, user etc. This behavior may change in the future.


.. note:: ``controller_file`` replaces ``controller_file_readonly`` and ``controller_lib``

.. seealso:: :ref:`controller-file_id`, :ref:`tag-lib`, :ref:`tag-image`, :ref:`tag-image_url`

.. versionadded:: 0.11
