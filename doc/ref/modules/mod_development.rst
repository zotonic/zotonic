
.. include:: meta-mod_development.rst

Presents various tools for development.

.. note:: **Do not enable this module on production systems, as it severely impacts performance.**


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



Automatic recompilation
-----------------------

When this module is enabled, it runs an `inotifywait` program in the
background, which watches files in Zotonic and its site for
changes. If it detects changes, it performs certain actions.

* If an `.erl` file changes, it recompiles and reloads the file
  on-the-fly. When a module exports notifier functions (`observe_...`
  or `pid_observe_...` functions), these are re-registered with the
  notification system automatically.

* If a `.scss` or `.less` file is touched, it calls ``lessc`` to compile
  it to its `.css` equivalent.

* If a template file is added, or a dispatch rule changed, it flushes
  the cache so the template file or the dispatch rule is found.

.. note:: Automatic recompilation currently only works on Linux and depends on the `inotifywait` tool, which is part of the ``inotify-tools`` package.


Configuration options
---------------------

``mod_development.libsep``
   Boolean value. If true, :ref:`tag-lib` files will be included separately instead of in one big concatenated file.

