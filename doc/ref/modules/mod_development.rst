
.. include:: meta-mod_development.rst

Presents various tools for development.

Admin page
----------

After the development module is enabled a menu item `Development` is added to the `System` menu in the admin.

On the development page it is possible to set debugging options, trace template compilation, and test dispatch rules.

Options
.......

This can toggle various development options:

Show paths to included template files in generated templates
    Checking this will add comments in the compiled templates. The comments will list the exact
    file included at that point.

Show defined blocks in generated templates
	Checking this will add comments in the compiled templates. The comments will show the start and
	end of any template ``{% block %} ... {% endblock %}``.

Download css and javascript files as separate files (ie. don’t combine them in one url).
	Checking this will generate separate ``<link/>`` and ``<script/>`` tags for all files
	mentioned in a single ``{% lib %}`` tag. This makes debugging those files easier but makes
	loading pages slower as more requests will be done per page.

Enable API to recompile & build Zotonic
	The api on ``/api/development/recompile`` can be accessed to trigger a full compilation and cache flush
	of Zotonic. This checkbox must be checked to enable this api.


Template debugging
..................

The template selection mechanism is quite complicated. It takes into account all modules, their priority,
the user-agent class (desktop, tablet, phone or text) and optionally the category of a resource.

With this debugging tool you can optionally select a category, and fill in the name of the template.
Per user-agent class the selected template will be shown.

.. image:: /img/development_template_debug.png


The second debug option is a page with a live display of all templates being compiled.
With this it is possible to get greater insight in the template selection and compilation.


Dispatch rule debugging
.......................

With this it is possible to see for a request path which dispatch rules are matched and/or how it is rewritten.

.. image:: /img/development_dispatch_debug.png


.. _automatic-recompilation:

Automatic recompilation
-----------------------

The core Zotonic system starts either ``inotify-tools`` or ``fswatch``, depending on
which one is available. You have to install one of these to enable auto-compile
and auto-load of changed files.

.. note:: The system can only scan for changed files if either ``inotify-tools`` or ``fswatch`` is installed.

See below for platform-specific installation instructions.

If a changed file is detected then Zotonic will:

* If an `.erl` file changes then the file is recompiled.

* If a `.scss` or `.sass` file changes then ``sass`` is called to compile
  it to its `.css` equivalent.

* If a `.less` file changes then ``lessc`` is called to compile
  it to its `.css` equivalent.

* If a `.coffee` file changes then ``coffee`` is called to compile
  it to its `.js` equivalent.

* If a lib file changes then the module indexer will be called so that any
  removed or added templates will be handled correctly.

* If a template file changes then the module indexer will be called so that any
  removed or added template will be handled correctly.

* If a dispatch file changes then all dispatch rules are reloaded.

* If a beam file changes then the module will be loaded. If the beam file is
  a Zotonic module then it will be automatically restarted if either the
  function exports or the ``mod_schema`` changed.

* If the .yrl definition of the template parser changes, then the .erl version
  of the parser is regenerated. (This will trigger a compile, which triggers a
  beam load).


Linux installation
...................................................

On Linux this feature depends on the `inotifywait` tool, which is part
of the ``inotify-tools`` package. For displaying notifications, it
uses ``notify-send``::

  sudo apt-get install inotify-tools libnotify-bin


Mac OS X installation
........................................

On Mac OS X (version 10.8 and higher), we use the external programs ``fswatch`` and
``terminal-notifier``::

  sudo brew install fswatch
  sudo brew install terminal-notifier


Configuration options
---------------------

``mod_development.libsep``
   Boolean value. If true, :ref:`tag-lib` files will be included separately instead of in one big concatenated file.

