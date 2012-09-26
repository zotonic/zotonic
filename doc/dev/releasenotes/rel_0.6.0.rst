Release 0.6.0
=============

Released on 2011-02-12.


New features
------------

SSL support
   Zotonic has gotten support for serving web pages over secure HTTPS
   connections. When configured, it listens by default on port 8443.
   See http://zotonic.com/https-support for details.

z_logger 
   A new subsystem for the low-level logging and tracing of requests.
   This module should be used to log lower level events during
   development time. Higher-level log messages (e.g. events by Zotonic
   modules) are still handled by 'mod_logging'.

multilingual content
   Every resource can have be translated in as many languages as you
   like. The admin has gotten an interface to provide the editing of
   the multiple language versions. Available languages are fully
   dynamically configurable.

z_depcache
   Partial rewrite of depcache system, is now faster and using more
   the process dictionary of the calling process to cache often used
   values.


New and changed modules
-----------------------

mod_signal
   New module providing a handy signal and slot mechanism for use in
   templates.

mod_tkvstore
   New module providing a simple typed key/value store for modules and
   Erlang code.

mod_translation
   Check if the user has a preferred language (in the user's
   persistent data). If not then check the accept-language header (if
   any) against the available languages.

mod_mailinglist
   Tweaks in the templates, updated dutch translations; do not send
   mail when deleting recipient from admin; Added 'recipient_id' to
   some e-mails so that the e-mails are sent in the correct language.

mod_authentication
   Fix user name display in password reminder e-mail.

mod_emailer
   Fix for e-mail override, escape the '@' in the original e-mail
   address.  Added flushing of poll messages

mod_seo
   Added option to set a no-index for a complete site.  New Google
   Analytics tracker code. With thanks to Richard Fergie.

mod_contact
   Configurable from address for contact email

mod_admin_identity
   Fix for finding users, select only identity records with type
   'username_pw'

mod_calendar
   Better handling for undefined date_end values.

mod_search
   Improper months ordering in archive_year_month query. (#134)

mod_menu
   Possibility to create an arbitrary number of different menu's. Also
   a new filter (menu_trail) which gets the menu trail for the main
   menu.


Changes to template filters and tags
------------------------------------

'first' filter
   added optional length parameter

min/max and minmax
   3 new filters were added to clamp a value in an (integer) range.

filesizeformat 
   New filter, similar to the Django filesizeformat filter.

lib tag
   Extended the lib tag with a 'use_absolute_url' option.

confirm/alert actions
   These actions were changed and now use HTML dialogs instead of
   javascript popups.

reversed
   New filter to reverse a list

menu tag
   Added 'menu_id' parameter to specify which menu to render

date_diff
   New filter to calculate the difference between two dates

tinymce_add, tinymce_remove
   New actions to dynamically initialize of de-initialize rich
   textareas

trigger_event
   New action to trigger a named wire.

wire
   Added a new 'visible' wire type, which triggers when the wired
   element comes into view (by scrolling or using 'show').

lazy
   New scomp which shows a 'loader' image and performs onetime actions
   when loader comes into view.



General bug fixes
-----------------

- Fix for 'double-dot' in e-mails when using postfix. Also encode the $. characters using quoted-printable.
- Fix for format_price filter. Show thousands when no cents.
- Make video embed code editable.
- Merged various webmachine fixes, updating it to 1.7.3:
 - support {stream, TotalSize, StreamFun} body result for range-capable streams
 - Add infinity timeout to gen_server calls
 - Allow multiple IP/port bindings
 - split chunk header on semicolon just in case a client is using chunk extensions
 - properly extract peername from all rfc1918 addrs
 - change H7 to match on any if-match, not just *
 - webmachine: WM-1.7.3(compat) ignores client's Content-Type on HTTP PUT requests (#130)
 - webmachine: prevent using chunked transfer encoding with HTTP/1.0.
- increase the startup timeouts for the gen_servers to prevent startup race condition
- Update mochiweb to latest version from mochi/mochiweb github repository (1.5.0)
- Pulled latest epgsql driver to support Postgres notifications.
- Added additional mime types (Office 2007, .rar)
- z_session: Only mark the persistent store as dirty when a persistent value changes.
- pgsql: Fix for a problem where a postgres connection was not returned to the pool in case of a sql error.
- z_media_preview: some files without a preview where not showing an icon.
- fixed an DoS vulnerability in Mochiweb/SSL
- Added flushing for most periodic internal messages (e.g. tick, poll)
- windows: fix build.cmd; remove some unix-specificness from imagemagick shell commands
- mochiweb: Cookie expire date format string now follows rfc2109
- ACL checks on static file serving
- Comet: support for cross-domain comet connections
