Release 0.5.0
=============

Released on 2010-10-03.


New features
------------

Simpler module system 
   Modules are simpler, do not have to be a fullblown
   gen_server. Registering z_notifier for modules is made more simpler
   by using erlang's introspection on modules.

i18n support through gettext
   Gettext .po and .pot file support for translations. Templates can
   be translated per module.  Pot files are automatically generated
   from the templates.

Pluggable Access Control system
   The new ACL structure works through pluggable ACL modules. Two ACL
   modules are included as examples.  mod_acl_adminonly, where all
   users are admins, and mod_acl_simple_roles, which implements a
   simple role based ACL system.

Authentication can now be customized and extended.  
   mod_authentication is the basic module used for
   authentication. This module can be extended. The mod_facebook is an
   (incomplete) example of such an extender. mod_authentication
   implements the username/password authentication, including logon
   and logoff.  It also supports 'password forgotten' e-mails.

User signup
   Non admin users can sign up using the mod_signup. This module works
   in harmony with the authentication module and authentication
   extenders.

New OTP supervisor hierarchy. 
   The PostgreSQL connection pool is now part of the individual
   sites. Sites are more isolated and can be individually started,
   restarted or stopped. It is possible to add and remove sites
   without restarting Zotonic.  Modules are now isolated and the
   running status of a module is displayed in the admin's module
   overview.

A status overview site, zotonic_status.  
   zotonic_status shows the running status of all sites.  When logged
   in, the user can start/stop/restart sites using his browser.  It is
   also possible to do 'hg pull' updates of sites that contain a
   mercurial repo.
  
New ErlyDTL filters
   group_by_title_firstchar, is_visible, pprint, urlize,
   without_embedded_media.

Media preview enhancements
   {% image %} now supports the the following new arguments:
   
   'extent' - create a larger image then the original not by scaling
   up but by adding a border to the image.
   
   'removebg' - removes the image's
   background. It accepts an optional fuzziness parameter (range
   0..100).

   'upscale' - Forces a small image to scale up to the requested
   dimensions.

Extended support for Websocket connections. 
   The two newest protocols, as used by Chrome and Safari, are
   supported.

mod_development improvements
   It now supports turning on or off the concatenation of {% lib %}
   includes as one file or separate files, and can give a live trace
   of translated templates, showing clearly the template inheritance
   and selections.

mod_menu improvements
   It implements the menu now as a template, easing your own menu
   implementation.

mod_emailer improvements
   It can now inline images into the e-mails

New: mod_slideshow 
   It can make a slideshow of any collection, you can add your own
   slide templates.

New: mod_contact
   Simple contact form which gets sent over e-mail

New: mod_facebook
   Facebook logon

New: mod_imageclipper
   A simple javascript image-clipper bookmarklet for grabbing images
   from other websites.

New: mod_logging
   A realtime log of debug messages and errors in the system.

Other features:

* New ErlyDTL tags: {% inherit %}, {% overrule %}
* New ErlyDTL support for multiple argument {% with %}: {%  with a,b as c,d %}
* New ErlyDTL support for filters with multiple parameters.
* New ErlyDTL test set, including regression tests.
* System wide configuration system (z_config) using a configuration file at 'priv/config'


Bugfixes
--------

* Allow HTML5 audio and video tags (#75)
* Typo in m_config, line 127. undefind -> undefined (#83)
* setting initial admin password does not work (#88)
* After upgrading the code to latest changeset admin authentication causes exception (#91)
* Menu module does not follow ACL rules  (#92)
* Crash in start.sh using Erlang R14A on Mac OS X 10.6 (#93)
* Extra Atom Link (#95)
* Makefiles use rm GNUism (#96)
* z_email:split_name_email/1 does not what it says it should do (#97)
* dots in page paths are transformed into dashes  (#98)
* attaching media to pages does not work correctly (#99)
* After a module crashes, the new dynamic observe_* methods are not re-initialized (#100)
* setting page path and unique name is broken (#101)
* IF statements on empty rsc_list structures (#104)
* When image is too small, providing only a width should not make the image very large (#105)
* And many various other fixes which users noted on the mailinglist and were fixed quickly.
