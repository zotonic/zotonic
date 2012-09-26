Release 0.7.0
=============

Released on 2011-07-28.


New core features
-----------------

SMTP
   Native SMTP support for sending and receiving e-mails in any
   Zotonic site. We integrated Andrew Thompson's gen_smtp library
   which allows us to manage outgoing ánd incoming mails. mod_logging
   provides a new email log-view for inspecting what mails go in and
   out.

Commandline   
   A "zotonic" shell command. The "zotonic.sh" shell command has been
   replaced by a more generic and more powerful shell command with
   support for pluggable subcommands.

Module repository
   Zotonic now supports installing system-wide modules which are not
   part of the core repository. We have created a place where
   externally contributed modules can be linked at
   http://modules.zotonic.com/. Modules registered on that site can be
   easily installed through the "addsite" subcommand.

Skeleton sites
   The default website has been replaced by the notion of "skeleton"
   sites. The "zotonic addsite" command lets you create a new Zotonic
   website based on one of the (currently two) website templates.


New modules
-----------

mod_email_relay
   Relay received e-mails to an user's email address. Serving as an
   example for the SMTP functionality, this module looks up a username
   by the local part of a received e-mail and forwards the mail to the
   mail address the user configured.

mod_email_receive
   Handle received e-mails, notifies email observers depending on a
   stored mapping of recipient addresses.

mod_import_csv
   Fairly generic module for importing CSV files, updating or creating
   new content on the fly.

mod_import_wordpress
   Basic import module for Wordpress WXR file format, allowing you to
   migrate a Wordpress blog into Zotonic.


Discontinued modules
--------------------

To make Zotonic more lightweight and remove some of the build
dependencies, some infrequently used modules have been removed from
the core and moved to their own repository, at
http://code.google.com/p/zotonic-modules/.  These modules are
mod_search_solr, mod_pubsub, mod_slideshow, mod_broadcast,
mod_imageclipper, mod_admin_event and mod_calendar. They can still be
easily installed with the help of the "zotonic installmodule"
command. The mod_emailer module (and its esmtp library) has been
removed in favor of the native SMTP sending/receiving capabilities.

Each module now also can have its own dependencies by including a
"deps" subfolder in the module. This is used for example in the
mod_pubsub external module which has the exmpp library as a dependency.


Other minor features
--------------------
* to_json filter for representing template values as JSON objects
* split filter for splitting a string
* slice filter for manipulating lists
* Added {% raw %}..{% endraw %} support for representing literal code blocks.
* erlydtl: Added possibility to define atoms using backquoted strings.
* admin templates are reorganized, allowing to write admin customizations with less code
* translations of the admin updated and more translations added


Bugfixes
--------

Too many bugfixes to list. However, the base system is becoming more
stable and this release aims to be a good step towards the 1.0.
