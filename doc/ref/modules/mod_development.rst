
.. include:: meta-mod_development.rst

.. todo:: Not yet completely documented.

Presents various tools for development. When this module is enabled,
templates are recompiled on each web request.

.. note:: **Do not enable this module on production systems, as it severely impacts performance.**

Automatic recompilation
-----------------------

When this module is enabled, it runs an `inotifywait` program in the
background, which watches files in Zotonic and its site for
changes. If it detects changes, it performs certain actions.

* If an `.erl` file changes, it recompiles it on-the-fly.

* If a `.scss` or `.less` file is touched, it calls ``lessc`` to compile
  it to its `.css` equivalent.

* If a template file is added, or a dispatch rule changed, it flushes
  the cache so the template file or the dispatch rule is found.

.. note:: Automatic recompilation currently only works on Linux and depends on the `inotifywait` tool, which is part of the ``inotify-tools`` package.

Configuration options
---------------------

``mod_development.libsep``
   Boolean value. If true, :ref:`tag-lib` files will be included separately instead of in one big concatenated file.

