
.. include:: meta-static_pages.rst

Serve a static page or pages.

With this controller it is possible to add a folder with static files as a sub-site to your
Zotonic site. Add the folder and all files to a directory in your
template directory or your site’s directory and define the directory
in a dispatch rule.

Example dispatch rule::

  {oldsite, ["old", '*'], resource_static_pages, [{root, "old_site"}]}

When a file ``a.txt`` is requested this resource will check for
``a.txt`` and ``a.txt.tpl``.  When it finds a ``.tpl`` file then that
file be handled as a template.  All dispatch configuration variables
are available in the template.

Directories will be redirected to the directory name with a ``/``
appended.  The resource serves the file ``index.html`` or
``index.html.tpl`` for the directory contents. If these are not found,
it will give a 404 page, unless the ``allow_directory_index`` option
is set; in which case a directory listing is displayed.

It has the following dispatch options:

+---------------------+-------------------------------------+-----------------------+
|Option               |Description                          |Example                |
+---------------------+-------------------------------------+-----------------------+
|root                 |Name of the directory in the site    |{root, "oldsite"}      |
|                     |directory containing the static      |                       |
|                     |files. The root is a path name       |                       |
|                     |relative to the current site's base  |                       |
|                     |directory.                           |                       |
+---------------------+-------------------------------------+-----------------------+
|use_cache            |Whether or not served files are      |{use_cache, true}      |
|                     |cached in memory for an              |                       |
|                     |hour. Defaults to false. Use this    |                       |
|                     |for high-volume traffic when the     |                       |
|                     |files themselves do not change       |                       |
|                     |often.                               |                       |
+---------------------+-------------------------------------+-----------------------+
|allow_directory_index|Whether or not to serve a directory  |{allow_directory_index,|
|                     |listing when no index file is        |true}                  |
|                     |found. Defaults to false. The        |                       |
|                     |directory index is rendered using    |                       |
|                     |:ref:`template-directory_index`.     |                       |
|                     |                                     |                       |
|                     |.. versionadded:: 0.9                |                       |
+---------------------+-------------------------------------+-----------------------+

This resource does not handle any request arguments.
