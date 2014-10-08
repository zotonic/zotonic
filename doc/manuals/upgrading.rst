.. _upgrade-notes:

Upgrade notes
=============

These notes list the most important changes between Zotonic
versions. Please read these notes carefully when upgrading to a new
major Zotonic version.


Upgrading to Zotonic 0.12
-------------------------

Bootstrap CSS version 3
.......................

Zotonic has been switched over to the latest version of the Bootstrap
Framework. When you are using Zotonic's ``mod_bootstrap`` or when you
have customized the admin templates, you will need to update your
templates.

A full migration guide to upgrading from Bootstrap 2.x is here:
http://getbootstrap.com/migration/, a tool to help you convert your
Zotonic templates is located here:
https://github.com/arjan/bootstrap3-upgrader.



Upgrading to Zotonic 0.11
-------------------------

Global configuration changes
............................

The global file ``priv/config`` has been obsoleted in place of a new
global configuration file, ``~/.zotonic/zotonic.config``. 

To upgrade your config file, do the following:

 * Make a directory in your home folder, called ``~/.zotonic``.
 * Copy ``priv/zotonic.config.in`` to ``~/.zotonic/zotonic.config``
 * Copy any settings from ``priv/config`` into the new ``priv/zotonic.config`` (IP addresses, etc)
 * Remove the old file ``priv/config``, as it is no longer in use.
 * Also, move ``priv/erlang.config`` to ``~/.zotonic/erlang.config``.
  
These configuration files can also be put in other places
(``/etc/zotonic``, most notably), or can contain Zotonic's version
number or node name when running multiple Zotonic versions side by
side. See :ref:`manual-configuration` for all information on this
topic.

.. note:: You can *not* just copy over your old ``priv/config`` file to the new
          location, as the structure of the file has changed.


Changed location of sites and external modules
..............................................

The default place for user-defined sites and external modules has been
changed to the defaults ``user/sites`` and ``user/modules``,
respectively.

To move your sites and modules in the right places, do the following:

 * In the zotonic dir, do ``mkdir -p user/{modules,sites}``
 * Move any external modules: ``mv priv/modules/* user/modules/``
 * Move all sites except ``zotonic_status`` and ``testsandbox`` to ``user/sites``.

You can change the location of the user-defined sites and modules by
changing ``user_sites_dir`` and ``user_modules`` dir settings in the
:ref:`manual-configuration`.


Postback and javascript changes
...............................

The file ``zotonic-1.0.js`` now uses ``lib/js/modules/ubf.js``. This file **must**
be included for the Zotonic javascripts to work.

All postback, comet and websocket connection are now handled by ``z_transport``.
Check :ref:`manual-transport` for details.

The ``stream`` tag has been deprecated. You can remove it from your
templates. Zotonic now automatically starts a WebSocket connection on
each page, unless ``nostream`` is given in the :ref:`scomp-script` tag.


Dispatch rules for files
........................

The ``controller_lib`` and ``controller_file_readonly`` have been replaced
by the ``controller_file``. This controller uses the new *filestore* system 
in Zotonic. This enables the storage of files on remote services like S3.

If you have added your own ``controller_lib`` or ``controller_file_readonly``
dispatch rules then you have to change them to use ``controller_file`` instead.

The following options have been **removed**:

 * media_path
 * is_media_preview
 * use_cache
 * use of an *id* argument, use ``controller_file_id`` instead

See the documentation for :ref:`controller-file` and :ref:`controller-file_id`.


Modules moved out of core
.........................

The ``mod_geomap`` repository has moved to its own dedicated
repository. To keep using this module, you'll now need to install it
as an external module: ``zotonic modules install
mod_geomap``. Alternatively, you can try the module ``mod_geo``
(``zotonic modules install mod_geomap``) , which uses Google Maps in
the admin.


Database-driver changes
.......................

Due to the introduction of the new database driver, the behaviour of
automatically serializing Erlang terms into the database (on ``bytea``
columns) has been made explicit. To enable serialization of database
values, you have to tag them with the new ``?DB_PROPS(...)``
macro. Unserialization of terms is still done automatically.

Gotcha's
........

If you get this error on startup::

  DTREE: cannot open ''

You can fix this by doing: ``rm -rf deps/ua_classifier``, and then running ``make`` again.
  

Upgrading to Zotonic 0.10
-------------------------

Site config changes
...................

The site `hostalias` option has been changed to be a list of host
aliases instead of multiple pairs of hostalias attributes. Change your
site's configuration from this::

  {hostalias, "www.example.com"},
  {hostalias, "www.example.net"},
  {hostalias, "example.org"},

To this::

  {hostalias, ["www.example.com", "www.example.net", "example.org"]},

Besides this change, a site's config file can now also be split into
multiple files living under the ``config.d/`` folder within a site.

Build process
.............

The ``git`` tool is now **required** to build Zotonic, even when you
downloaded the release zip file. This is due to Zotonic's external
dependencies now being managed with the ``rebar`` tool.

  
Misc changes
............

All configuration options regarding logging are now in set in the ``priv/erlang.config`` file,
which is created by default if missing from ``priv/erlang.config.in``.



Upgrading to Zotonic 0.9
------------------------

CSS changes
...........

Due to the move to Bootstrap, the following CSS changes need to be
made in your templates:

+-------------------------------+---------------------------------+
| Old CSS selector              | New CSS selector                |
+-------------------------------+---------------------------------+
|``.sf-menu``                   |``.nav``                         |
+-------------------------------+---------------------------------+
|``.sf-menu a.current``         |``.nav li.active a``             |
+-------------------------------+---------------------------------+
|``ul.pager``                   |``div.pagination ul``            |
+-------------------------------+---------------------------------+


Controllers
...........

The Erlang modules formerly known as `Webmachine Resources`
(``resources/resource_*.erl``) have been renamed to
`controllers`. They live in the ``controllers/`` folder in a
module. This was done to eliminate the confusion between webmachine
resources and the "rsc" table of the Zotonic datamodel.

This means that you have to update your custom dispatch rules. Each
dispatch rule which uses one of Zotonic’s ``resource_*`` controllers,
needs to be changed from this::

  {article,      ["article", id, slug],      resource_page,      [ {template, "article.tpl"} ]},

to this::

  {article,      ["article", id, slug],      controller_page,      [ {template, "article.tpl"} ]},

et cetera.

Also, when you wrote your own controllers, you need to rename your
``resource_`` module to use the controller prefix, and make sure it uses
the new include file names.

The following include files have been renamed:

+-------------------------------+----------------------------------------+
|Old filename                   |New filename                            |
+-------------------------------+----------------------------------------+
|include/resource_html.hrl      |include/controller_html_helper.hrl      |
+-------------------------------+----------------------------------------+
|include/webmachine_resource.hrl|include/controller_webmachine_helper.hrl|
+-------------------------------+----------------------------------------+

HTTPS support
.............

HTTPS support was moved from the core into a new module, :ref:`mod_ssl`.

The global ``priv/config`` options ``ssl``, ``ssl_certfile``,
``ssl_keyfile`` and ``ssl_password`` do no longer have an effect. See
:ref:`mod_ssl` on how to configure HTTPS support for Zotonic from 0.9
and up.


Removed controller
..................

The under-used ``resource_home`` controller has been removed. Change
your dispatch rules accordingly to use ``controller_template``::

  {home,  [],  resource_home,       []},

to this::

  {home,  [],  controller_template, [{template, "home.tpl"}]},

Removed filters
...............

The ``lenght_is`` filter has gone. Replace constructs like this::

  {% if value|length_is:5 %}

to::

  {% if value|length == 5 %}


mod_backup
..........

mod_backup’s configuration values for binary path names (`pg_dump` and
`tar`) is now coming from the global ``z_config`` instead of the
site’s configuration database.

On startup you might see this message::

  18:39:59.895 [error] z_module_manager:485 [sitename] Error starting module mod_backup: {error,{missing_dependencies,[rest]}}

mod_backup is now dependent on mod_rest, so you should enable that module in the module manager.


mod_survey
..........

The storage format changed slightly. For the correct display of the
results of *narrative*-type questions answered before 2012-12-01, the
name of the block needs to equal the name of the first narrative
sub-question.


z_logger
........
On startup you might see this message::

  ** /home/zotonic/zotonic/deps/z_logger/ebin/z_logger.beam hides /home/zotonic/zotonic/deps/webzmachine/ebin/z_logger.beam
  ** Found 1 name clashes in code paths
  
z_logger has been moved from its own reps/z_logger repo into
webzmachine.  You can delete the entire ``deps/z_logger`` directory.

  
Upgrading to Zotonic 0.8
------------------------

Module versioning
.................

From 0.8, modules have a schema version concept, which is used to
install and update module-specific data (like managed tables, custom
categories, default data). Previously this was either done in the
module’s ``init()`` or ``datamodel()`` function. The ``datamodel/1``
function is no longer called upon module start.

Instead, modules export a ``-module_schema()`` attribute which
contains an integer number, denoting the current module’s version. On
module initialization, ``Module:manage_schema/2`` is called which
handles installation and upgrade of data. See :ref:`manual-modules`
for more information and example code.

mod_mailinglist
...............

The mailinglist has changed a bit. You need to manually enable the
``mod_logging`` module on upgrade. It should be enabled automatically,
but please double-check.

Execute the following query to get email sending working::

  alter table mailinglist_recipient add column is_bounced boolean not null default false;


Upgrading to Zotonic 0.7
------------------------

Removed modules
...............
To make Zotonic more lightweight and remove some of the build
dependencies, some infrequently used modules have been removed from
the core and moved to their own repository, at
http://code.google.com/p/zotonic-modules/.  These modules are:

* mod_search_solr
* mod_pubsub
* mod_slideshow
* mod_broadcast
* mod_imageclipper
* mod_admin_event
* mod_calendar
* mod_emailer*

All modules, except mod_emailer can still be easily installed with the
help of the ``zotonic modules install`` command. The mod_emailer module
(and its esmtp library) has been removed in favor of the native SMTP
sending/receiving capabilities.  

New SMTP architecture
.....................

The mod_emailer module has been removed in favor of a separate mail
server process and queueing system. For more information please read
the e-mail configuration page in the documentation.

The ``emailq`` table has become obsolete. You can remove the table from
your existing Zotonic database.

Admin password
..............

The admin password is now hardcoded in your site’s config file. For sites that are upgrading, you have to add a line to your config file::

  {admin_password, "letmein"}

The value in the config file always reflects the current admin
password (as opposed to zotonic < 0.6!) and thus the admin password
can only be changed by changing it there.

Admin extra richtext fields
...........................

If you have extra richtext (tinymce) fields in the admin, you need to
rename the class tinymce of the textarea to the class name
tinymce-init.


Upgrading to Zotonic 0.6
------------------------
No notable upgrade measures need to be taken.

Upgrading to Zotonic 0.5
------------------------

Some filters disappeared and changed into expression syntax: ``|eq``,
``|ne``, ``|lt``, ``|gt``, ``|not``, etc.:

``{% if id|eq:2 %}`` becomes ``{% if id == 2 %}``
``{% if id|not %}`` becomes ``{% if not id %}``
et cetera.

The meaning of the query filters `hassubject`, `hasobject`,
`hassubjectpredicate` and `hasobjectpredicate` has been reversed::

  m.search[{query hasobject=id}]           

becomes::

  m.search[{query hassubject=id}]          

and reverse::

  m.search[{query hasobjectpredicate=id}]  

becomes ::

  m.search[{query hassubjectpredicate=id}] (and reverse)


``resource_staticfile’s`` ``root`` directory has changed from the site’s template folder to the sites base folder, e.g. from `site/templates/xx` to `site/xx`.

The `m_group`` model no longer exists.

When you first install zotonic and want to logon into /admin, you dont
need to give a password, just the username, 'admin'. It will then ask
you to set the admin password.

User accounts need to be published otherwise their logon will be
denied. Use this query to enable every user in the database::

	update rsc set is_published=true 
	where category_id in 
		(select distinct(id) from rsc where name='person')

If you have an overruled base template, make sure that a {% block
content_area %} that spans the full width if your site is in there,
because this is used to render the logon dialog for the admin.
