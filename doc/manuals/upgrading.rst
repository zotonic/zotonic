Upgrade notes
=============

Although we try to keep things backward compatible, sometimes this is
just impossible, e.g. when renaming things for the sake of clarity.

These notes list the most important changes between Zotonic versions
that require attetion for site developers when things change in
incompatible ways or get deprecated.


Upgrading to Zotonic 0.9
------------------------

Controllers
...........

The Erlang modules formerly known as `Webmachine Resources`
(``resources/resource_*.erl``) have been renamed to
`controllers`. They live in the ``controllers/`` folder in a
module. This was done to eliminate the confusion between webmachine
resources and the "rsc" table of the Zotonic datamodel.

This means that you have to update your custom dispatch rules. Each
dispatch rule which uses one of Zotonic's ``resource_*`` controllers,
needs to be changed from this::

  {article,      ["article", id, slug],      resource_page,      [ {template, "article.tpl"} ]},

to this::

  {article,      ["article", id, slug],      controller_page,      [ {template, "article.tpl"} ]},

et cetera.

Also, when you wrote your own controllers, you need to rename your
``resource_`` module to use the controller prefix, and make sure it uses
the new include file names.

The following include files have been renamed:

+-------------------------------+---------------------------------+
|Old filename                   |New filename                     |
+-------------------------------+---------------------------------+
|include/resource_html.hrl      |include/html_controller.hrl      |
+-------------------------------+---------------------------------+
|include/webmachine_resource.hrl|include/webmachine_controller.hrl|
+-------------------------------+---------------------------------+

HTTPS support
............
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


Upgrading to Zotonic 0.8
------------------------

Module versioning
.................

From 0.8, modules have a schema version concept, which is used to
install and update module-specific data (like managed tables, custom
categories, default data). Previously this was either done in the
module's ``init()`` or ``datamodel()`` function. The ``datamodel/1``
function is no longer called upon module start.

Instead, modules export a ``-module_schema()`` attribute which
contains an integer number, denoting the current module's version. On
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
help of the ``zotonic installmodule`` command. The mod_emailer module
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

The admin password is now hardcoded in your site's config file. For sites that are upgrading, you have to add a line to your config file::

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


``resource_staticfile's`` ``root`` directory has changed from the site's template folder to the sites base folder, e.g. from `site/templates/xx` to `site/xx`.

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
