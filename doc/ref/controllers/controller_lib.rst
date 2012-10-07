
.. include:: meta-lib.rst

Serve css, javascript and simple images.

This controller is used to serve files and images, which are located
under the the ``lib/`` folder of modules and sites.

Multiple files can be served as one file; the controller can combine them in one single
request. See the :ref:`tag-lib` tag for more information.  Javascript files
are concatenated with a semi-colon and newline, all other files are
concatenated with a newline character between the files.

This controller serves all files with a very long client-side caching
time. It also handles ``if-modifies-since checks``, and sends ``301
Not Modified`` responses when appropriate. It alsocompresses the
served files using gzip when the browser supports it.

When serving multiple files, the `Modified:` response header will be set
to the modification date of the newest file.

Example dispatch rule::

  {lib, ["lib", '*'], controller_lib, []}

`controller_lib` has the following dispatch options:
  
+-------------------+-----------------------------------------+---------------------+
|Option             |Description                              |Example              |
+===================+=========================================+=====================+
|root               |List of root directories where files are |{root,               |
|                   |located. Use 'lib' for the library       |["/var/www/css/"]}   |
|                   |files. This defaults to the lib          |                     |
|                   |directory.                               |                     |
+-------------------+-----------------------------------------+---------------------+
|content_disposition|If the file should be viewed in the      |{content_disposition,|
|                   |browser or downloaded. Possible values   |inline}              |
|                   |are inline and attachment. Defaults to   |                     |
|                   |the browser’s defaults by not setting the|                     |
|                   |“Content-Disposition” response header.   |                     |
+-------------------+-----------------------------------------+---------------------+
|use_cache          |Use server side caching of               |{use_cache, true}    |
|                   |files. Especially useful when            |                     |
|                   |gzip-compressing files or serving many   |                     |
|                   |combined css or javascript files. Less   |                     |
|                   |useful when a proxy cache like Varnish is|                     |
|                   |used. Defaults to false.                 |                     |
+-------------------+-----------------------------------------+---------------------+

`controller_lib` does not handles any query arguments other than the file path.

.. seealso:: :ref:`controller-static_pages`, :ref:`controller-file_readonly`
