Release 0.12.2
==============

Released on 2014-12-18 17:01 by mworrell.

Major changes are:

* Addition of new notifications for importing media.
* Authentication using mod_twitter and mod_facebook is now via a popup window
* mod_twitter can now follow keywords and multiple users
* New admin menu item **Auth -> App Keys & Authentication Services**


The following commits were done
-------------------------------

Arjan Scherpenisse (3):

* build: force rebar to build 'setup' app
* mod_admin: Fix link to query documentation
* mod_search: Use binary parsing of stored query resource


Arthur Clemens (79):

* admin: button dropdowns for admin overview filters
* admin: fix checkbox in language dropdown
* admin: fix erroneous rule
* admin: fix insert depiction dialog
* admin: fix negative margins of editor
* admin: fix nested links in table
* admin: fix nested treelist
* admin: fix tab order of form fields
* admin: formatting
* admin: handle multiple lined tree list items
* admin: hide redundant Category text
* admin: improve display of thumbnails
* admin: improve interface in various locations
* admin: increase y position of fake form fields
* admin: make long filenames visible
* admin: make muted a bit lighter
* admin: minimal design of help buttons
* admin: more horizontal forms
* admin: more reordering of less styles
* admin: multiple css fixes forms and edit blocks
* admin: NL translation fixes
* admin: prevent nesting modal-body
* admin: refactor tabs
* admin: remove @baseFont reference
* admin: remove extraneous space
* admin: remove periods from dialog headers
* admin: remove unused file
* admin: tweak admin login in release branch
* admin: use translated string
* admin: various markup improvements
* doc: add action set_class
* mod_acl_simple_roles: add titles to modules and category names
* mod_acl_simple_roles: fix div nesting
* mod_acl_simple_roles: improve modules list
* mod_acl_simple_roles: NL typo
* mod_acl_simple_roles: remove period from header
* mod_admin: allow table parts to be not clickable
* mod_admin: also update dashboard button row
* mod_admin: block menu is right aligned
* mod_admin: bootstrap 3 uses class text-muted
* mod_admin: break apart dashboard blocks for easier overriding
* mod_admin: consistent New Page dialog header
* mod_admin: css tweaks
* mod_admin: enable "All pages" button when qcat is defined
* mod_admin: handle empty cat param value
* mod_admin: improve stacked elements within widgets
* mod_admin: layout and translation of dialog Duplicate Page
* mod_admin: make button dropdown alignment configurable
* mod_admin: make white borders in media visible
* mod_admin: more horizontal forms
* mod_admin: NL typos
* mod_admin: organize less styles in separate files
* mod_admin: preserve query string when changing category
* mod_admin: prevent disappearing of media images after undo
* mod_admin: remove period from logged in user
* mod_admin: show category and query in window title
* mod_admin: show crop center in connection overview
* mod_admin: show unpublished state in connected media
* mod_admin: smaller crop center lines
* mod_admin: support em-based buttons in button row
* mod_admin: update table row hover, make colors consistent
* mod_admin: use close button without glyphicon element
* mod_admin_config: consistent field widths
* mod_authentication: remove period from header
* mod_backup: typo
* mod_backup: use bullet list for warnings
* mod_base: document params width and addclass in js source
* mod_base: fix dialog text
* mod_base: missing button style
* mod_base: remove unused line of code
* mod_base: support dialog option backdrop "static"
* mod_base: tweak debug info
* mod_editor_tinymce: add newest version
* mod_editor_tinymce: add option to always use the newest version
* mod_editor_tinymce: if no config, use newest
* mod_editor_tinymce: limit autoresize; fix resize handle bug
* mod_oembed: activate upload button
* mod_seo: optimize descriptions
* mod_signup: remove period from link


Maas-Maarten Zeeman (10):

* Changed the websocket implementation.
* core: Added recon application
* core: Fix: return default when there is no session.
* deps: Updated mochiweb, ip-log fix for R15
* deps: upgraded sendfile
* deps: Upgraded webzmachine and mochiweb
* mod_base: More careful websocket handshake.
* mod_base: Removed commented out code.
* mod_seo: Make noindex and notrack configurable from templates
* Removed comment to fix edoc generation


Marc Worrell (47):

* Add docs
* core: add exceptions for .xls and .xlsx files to z_media_identify. Fixes #893
* core: added comment explaining expire_1 and expire_n for sessions. Issue #881
* core: allow a non-integer category id to be passed to all_flat/2
* core: allow setting any rsc property that is 'undefined' to 'false'.
* core: ensure all db timestamp columns have a time zone.
* core: fix args for transport ack.
* core: fix problem where mod_signed_url could not keep the user logged on.
* core: fix problem where the custom redirects form was not saved
* core: fix specs in z_db.
* core: make session cookie name configurable (solves problems where old cookies might interfere, especially on Chrome)
* core: on context prune-for-scomp, leave an empty list for request headers. Normalize user-agent lookup.
* core: removed debug from z_pivot_rsc
* core: the {% script %} tag has now arguments.
* core: z_search: fix acl check sql query.
* Create database when starting site
* docs: adapt docs to changes in files.
* install: use openssl to generate the admin password, as tr/urandom combo hangs on OS X. Fixes #847
* Make menu_subtree compatible with names
* media/embed: fixes for twitter streaming, added notifications for importing and analyzing fetch url media-data.
* mod_admin/mod_admin_frontend: preparations to allow creation of resources via the edit page.
* mod_admin: remove api dispatch rule, also defined in mod_base.
* mod_admin: return the correct context in controller_admin_media_preview
* mod_admin_frontend: fix a problem where combining the nestedSortable.js lib with other js files will result in errornous drag behaviour
* mod_authentication: export send_reminder/2 and lookup_identities/2.
* mod_authentication: fix logon_box form input "password"
* mod_authentication: Refactor twitter/facebook logon and signup code.
* mod_base: do not redirect if redirect id is set to undefined
* mod_base: fix js error in livevalidation.
* mod_base: for catinclude, don't assign passed categories to 'id'. Only assign a resource id to id.
* mod_base: in do_popupwindow use e.preventDefault() to play nice with multiple click event listeners.
* mod_base: remove extra </div> from phone/_navbar
* mod_email_receive: when adding recipiens, catch references to non existing rsc
* mod_facebook: add delegate for saving settings.
* mod_menu/admin_frontend: final fix for new-page topic on menu insertion.
* mod_menu: correct pubzub.publish topic.
* mod_menu: remove console.log message.
* mod_mqtt: fix js error in for loops.
* mod_mqtt: fix problem where removing one topic listener removed all listeners. Cleanup live subscriptions for removed elemnts.
* mod_oembed: don't display the media twice in the admin
* mod_oembed: remove http: protocol from embed html, this enables content to be viewable on https: pages.
* mod_search: allow dates like 'now' and '+2 weeks' in search questions.
* mod_survey: allow printable overview if user has edit rights.
* mod_translation: add more rtl languages.
* mod_twitter: fix edoc problem.
* Remove is_integer check for cc5d94
* smtp: more relaxed error handling for spamd errors.
