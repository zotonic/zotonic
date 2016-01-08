.. _rel-0.13.0:

Release 0.13.0
==============

Welcome Zotonic 0.13.0, released on July 3, 2015.

Major changes are:

* New ACL module: mod_acl_user_groups (see below)
* New module: mod_content_groups
* New model: m_hierarchy, for category, content_group and user_group hierarchies
* Category renumbering is now a lot faster and more incremental
* Addition of Material Design art work
* New module: mod_email_status
* New module: mod_component
* DNSBL checks for incoming emails
* More strict sanitization of content
* Social integration with Twitter, Facebook, LinkedIn, and Instagram
* *is_dependent* Resources
* Refactored automatic compile and load of changed files

This release also incorporates all fixes in the 0.12.x branch.

A complete *git shortlog* since the 0.12.0 release is added below these release notes.

Documentation
-------------

Unfortunately we still miss some documentation for the new modules.
This will be corrected as soon as possible.


New Access Control module
-------------------------

The new *mod_acl_user_groups* module adds rule based access control.

* All resources (pages) are assigned a content group.
* All users are member of zero or more user groups.
* Content groups are arranged in an hierarchy
* User groups are arranged in an hierarchy

Rules are defined between the user groups and the content groups.
Per rule the following properties can be set:

* User group
* Content group
* Category (or *all categories*)
* Privileges: view, edit, link, delete
* Deny flag (for a negative rule)

The collection of rules has an *edit* and *publish* version.
The *edit* version can be tested with a special url.
If all is accepted, then the *edit* version van be published.

The *edit* version can also be exported and imported.
This includes the full hierarchies of all user- and content groups.


Categories and m_hierarchy
--------------------------

The category hierarchy tables have been replaced by *m_hierarchy*.
This model defines named hierarchies of resources (pages).

If the categories are changed then the system needs to update the
*pivot_category_nr* field of all resources. With the introduction 
of *m_hierarchy* this renumbering is much more efficient and will 
only affect a minimal number of resources.

The *m_hierarchy* module is also used for the content- and user group
hierarchies, as used by the new *mod_acl_user_groups* module.


Email status
------------

The new module *mod_email_status* tracks for all outgoing email addresses:

* If emails are successfully sent
* Number of emails sent
* Number of errors
* Number of bounces
* Latest error message

With this it will be much easier to get feedback on all outgoing email.


DNSBL checks
------------

All incoming SMTP connections are now checked against two DNS block lists:

* ``zen.spamhaus.org`` (http://www.spamhaus.org/zen/)
* ``dnsbl.sorbs.net`` (http://dnsbl.sorbs.net/general/using.shtml)

The list of DNSBL servers checked can be configured using the ``smtp_dnsbl`` 
key in the ``zotonic.config`` file.


mod_component
-------------

This is an experimental module. It will be the basis of a new method of
loading *components* in HTML pages. Components consist of HTML, javascript,
and css. These parts can be dynamically loaded. Layout can/will be done
using *mithril.js*.

This module will undergo significant changes, so use at your own risk.
It is included to support some parties that use this in production.


Sanitization of content
-----------------------

``iframe`` Elements and CSS are now sanitized. The *iframes* are sanitized
using a whitelist of servers. Use the ``#sanitize_embed_url{}`` notification
to add or remove servers from the whitelist. The current whitelist can be
seen at the bottom of the ``src/support/z_sanitize.erl`` file.

*Object* tags referring to services like *youtube* are replaced with *iframe*
tags.

This sanitization is also done on all *embed* codes of media items.

The CSS parser is strict and only allows well formed CSS. It also strips
CSS that is known to be (potentially) *dangerous*:

* loading external content
* ``position: screen``
* some classes that are used internally in Zotonic
* and some more

Check ``deps/z_stdlib/src/z_css.erl`` for the actual sanitizer code.


Social integration
------------------

The integration with social sites is completely redone. It is now possible to
login (and signup) with Facebook, Twitter, LinkedIn, and Instagram.

It is also possible to import content from Twitter and Instagram. For this
it is possible to define tags and/or users to follow. All matching content
is imported into special categories. An optional image or embed code is also
imported and added *as part of* the imported resource (so not as a separate
depiction).


*is_dependent* Resources
------------------------

A new flag is added to all resources: *is_dependent*

If this flag is set, and a connection to the resource is deleted, then Zotonic will
check if there are any other *incoming* connections to the resource. If not then
the resource will automatically be deleted.

This makes it possible to have resources (images, documents) that can only exist
in the context of another resource. If that referring resource is deleted then the
depending resources are deleted as well.


Automatic compile and load of changed files
-------------------------------------------

The core system now starts either ``inotify-tools`` or ``fswatch``, depending on 
which one is available. You have to install one of these to enable auto-compile
and auto-load of changed files.

These are watching for any change to files in Zotonic and the sites.

If a file is changed then Zotonic will:

* Recompile if a .erl file changed
* Regenerate css if a .sass, .less or .scss file changed
* Regenerate js if a .coffee file changed
* Reindex the module index if a lib file changed (.css, .js, etc)
* Reload translations if a .po file changed
* Reload a module if a .beam file changed
* Reload dispatch rules if a dispatch file changed

If a Zotonic module is reloaded then it will be automatically restarted it the
function exports or the ``mod_schema`` is changed.



Commits since 0.12.0
--------------------

There were 586 distinct commits (more than 600 in total) since release 0.12.0.

The committers were:  Alain O'Dea, Arjan Scherpenisse, Arthur Clemens, David de Boer, Jeff Bell, 
Maas-Maarten Zeeman, Marc Worrell, Marco Wessel, Paul Guyot, Paul Monson, Sergei, Witeman Zheng, imagency, and 肥仔. 

Besides these commits many people contributed to this release.

Big thanks to all of you!


Git shortlog
............

For clarity, some cherry-picks and development branch merges are removed from this list.


Alain O'Dea (2):

* Fix#891
* scripts: Fix activating core / user modules

Arjan Scherpenisse (63):

* core: Move master up to 0.13-dev
* mod_base_site: Small Bootstrap 3 fixes
* core: Fix crashing make on initial build
* core: When using locked deps, still add the deps from zotonic.config
* scripts: Add "-v" argument to zotonic command to print the version
* doc: Fix preinstall notes about buggy erlang versions
* core: Disable sendfile support by default and make a note of this
* Add release notes for 0.12.1
* Add release notes for 0.11.1
* Merge pull request #856 from Yozhig/patch-1
* mod_search: Use binary parsing of stored query resource
* Merge pull request #857 from Yozhig/master
* mod_base: Add JSON source of the Zotonic logo font
* build: Add 0.11 and 0.12 release branches to travis
* build: Try to coerce Travis-CI not to run rebar get-deps by itself
* core: Prefix z_db error messages with site name
* mod_admin: Fix link to query documentation
* mod_acl_simple_roles: Fix ACL check when category of rsc cannot be found
* mod_twitter: Close login window when user denies login request
* mod_development: Enable automatic recompilation on MacOS X
* mod_twitter: Set all context vars when rendering logon_done template
* mod_twitter: Show logon page in current context's language
* mod_survey: Add 'submit' API service
* mod_video: Make command line to ffmpeg/ffprobe calls configurable
* core: Add 'preview_url' to rsc export API call
* mod_search: API: Add limit, offset, format arguments to search API
* core: use epgsql 2.0.0 instead of master
* mod_search: Add 'finished' search filter + documentation
* core: Fix edocs build
* mod_search: Add {finished} search method
* doc: Clarify that finished/upcoming filters don't perform sorting
* doc: Fix sphinx configuration
* Add release notes for 0.12.4
* mod_survey: Month names in dutch are lower case
* mod_content_groups: Put in 'structure' admin menu; fix dutch wording
* core: Remove category information from rsc export
* mod_content_groups: Put content group category in 'meta'
* mod_content_groups: Add .nl translation strings
* mod_acl_user_group: Initial version
* mod_search: Allow filtering resources on content group
* Add acl rule model
* Add ACL rule editor for module and rsc rules
* mod_survey: Add 'allow_missing' request parameter
* mod_admin: Place upload form in a block, allowing it to be easier overruled
* mod_admin: (CSS) file input buttons have varying heights on different OSs
* build: Bump Travis OTP version
* m_hierarchy: Add parents/3 function
* mod_acl_user_groups: Make ACL rules overview more usable
* Add edit basics dialog for user with tab to set user groups + options
* Let search box work on users page
* Search placeholder text
* Users overview: show all persons/institutions or only those with accounts
* Create import/export function for rules
* Fix compilation error in ACL rule export controller
* mod_development: Don't let sass create a source mapping
* mod_development: Don't let sass create a source mapping
* doc: Remove reference to Ubuntu PPA
* mod_survey: Fix results/printable page when survey rsc has page path
* mod_admin_identity: Remove big 'show all' button when searching users
* mod_admin_identity: Start with showing only users, not persons
* core: z_media_preview: Do not add white space to small images when resizing
* mod_base: Allow to set response headers from service API calls
* mod_base: Correct ReqData attribute in API controller on processing POST

Arthur Clemens (160):

* doc: update references to priv/config
* admin: fix negative margins of editor
* admin: make long filenames visible
* admin: button dropdowns for admin overview filters
* admin: remove @baseFont reference
* admin: remove unused file
* admin: fix nested links in table
* admin: prevent nesting modal-body
* admin: improve display of thumbnails
* admin: hide redundant Category text
* admin: increase y position of fake form fields
* admin: NL translation fixes
* admin: use translated string
* mod_admin: bootstrap 3 uses class text-muted
* admin: make muted a bit lighter
* mod_acl_simple_roles: improve modules list
* admin: fix tab order of form fields
* admin: improve interface in various locations
* admin: handle multiple lined tree list items
* mod_signup: remove period from link
* mod_authentication: remove period from header
* mod_admin: organize less styles in separate files
* mod_admin: block menu is right aligned
* admin: remove extraneous space
* admin: more reordering of less styles
* mod_seo: optimize descriptions
* admin: fix checkbox in language dropdown
* mod_backup: use bullet list for warnings
* mod_backup: typo
* mod_editor_tinymce: add newest version
* mod_editor_tinymce: add option to always use the newest version
* mod_editor_tinymce: limit autoresize; fix resize handle bug
* mod_admin: NL typos
* mod_base: missing button style
* mod_admin: break apart dashboard blocks for easier overriding
* mod_admin: css tweaks
* mod_admin: remove period from logged in user
* mod_editor_tinymce: if no config, use newest
* admin: multiple css fixes forms and edit blocks
* admin: multiple css fixes forms and edit blocks
* mod_admin: prevent disappearing of media images after undo
* admin: formatting
* mod_admin: use close button without glyphicon element
* mod_admin: more horizontal forms
* mod_acl_simple_roles: remove period from header
* mod_oembed: activate upload button
* mod_admin: make button dropdown alignment configurable
* mod_acl_simple_roles: add titles to modules and category names
* mod_base: fix dialog text
* admin: remove periods from dialog headers
* admin: minimal design of help buttons
* mod_acl_simple_roles: fix div nesting
* mod_admin: allow table parts to be not clickable
* mod_admin: update table row hover, make colors consistent
* admin: more horizontal forms
* mod_acl_simple_roles: NL typo
* admin: refactor tabs
* mod_base: document params width and addclass in js source
* mod_admin: preserve query string when changing category
* mod_base: tweak debug info
* mod_admin: improve stacked elements within widgets
* mod_authentication: refactor logon templates
* admin: fix insert depiction dialog
* admin: various markup improvements
* admin: fix nested treelist
* mod_admin: layout and translation of dialog Duplicate Page
* mod_admin: handle empty cat param value
* mod_admin: enable "All pages" button when qcat is defined
* mod_admin: consistent New Page dialog header
* mod_admin_config: consistent field widths
* mod_admin: also update dashboard button row
* doc: add action set_class
* mod_base: remove unused line of code
* mod_admin: show unpublished state in connected media
* mod_admin: make white borders in media visible
* mod_admin: show crop center in connection overview
* mod_admin: smaller crop center lines
* mod_base: support dialog option backdrop "static"
* mod_admin: support em-based buttons in button row
* mod_artwork: add font awesome 4
* mod_artwork: add zotonic logos
* doc: update logo and link style
* Merge pull request #896 from pmonson711/readme_fixes
* mod_artwork: rename logo folder to "zotonic"
* mod_base: extend logo font to include general icons for Zotonic modules
* admin: use zotonic icons
* mod_admin: remove unused files
* admin: general layout fixes
* zotonic_status: make responsive, update appearance
* fix gitignore
* zotonic_status: add less files
* mod_base: update favicon
* Remove unused files
* mod_base: simplify icon creation; add extending FA icons
* mod_base: replace some icons with FA versions, add button styles
* mod_admin: update with mod_base changes
* zotonic_status: notification tweaks
* mod_base: make extra tag more compatible
* mod_base: allow other libs to import z.icons.less
* mod_base: bring back (predictable) circle icons
* doc: typo
* mod_base: add non-circle icons
* mod_base: make icon extend cleaner
* mod_admin: mobile tweaks
* mod_base: hide original x char
* doc: restructure actions
* admin: include font-awesome version 4
* doc: typo
* mod_base_site: add icon css
* doc: sort lines
* doc: restructure filters
* basesite: remove old fix for logo image
* mod_filestore: use 0 if archive size is undefined
* mod_filestore: tidy up html
* Don't crash when email is undefined
* doc: formatting
* doc: Document template models
* mod_development: flush when translation file changes
* doc: remove deprecated %stream%
* mod_authentication: document template structure
* mod_authentication: remove superseded templates
* mod_authentication: better defaults for social login buttons
* mod_authentication: doc tweak
* mod_facebook: typo
* admin: tidy up Authentication Services page
* admin: fix zlink from editor, remove superseded template
* mod_admin: fix layout person form
* mod_admin: tidy email table
* Social login: use FB compliant "Login with..."
* mod_base: make Z icons independent of FontAwesome
* mod_base: include all font source files
* mod_base: add social login icons
* social login: use icons and update brand colors
* doc: add icons documentation
* mod_admin: remove confusing text
* mod_admin: layout tweaks
* translation tweaks (NL)
* mod_authentication: make login work in dialog
* mod_base: clean up documentation
* mod_admin: include icons css instead of recreating with less
* mod_authentication: shorter messages
* mod_authentication: layout tweaks
* mod_authentication: let admin logon use modal screens
* mod_authentication: default no box
* mod_base: add share icon
* mod_video_embed: add bootstrap responsive class
* mod_base_site: better defaults
* Revert "mod_video_embed: add bootstrap responsive class"
* doc: document 'page_url with' syntax
* doc: document more use cases for ``|if``
* mod_bootstrap: version 3.3.2
* doc: show css output when extending icons
* validation: provide message_after param to templates
* Authentication and signup: template refactoring
* doc: reword solution
* mod_artwork: add Material Design icons
* Fix log in verb
* mod_bootstrap: update update script
* mod_bootstrap: fix sourceMappingURL replacement
* mod_bootstrap: update to v3.3.4

David de Boer (10):

* Make menu_subtree compatible with names
* Remove is_integer check for cc5d94
* Create database when starting site
* Add docs
* Connect to "postgres" when creating database
* Fix page block links
* Fix modal signup
* mod_base: Allow to return 401 from API services
* mod_base: Decode JSON body in service calls
* Allow post-login redirect to be passed to logon modal

Jeff Bell (3):

* mod_admin:  Flushed out ability to save [object, predicate] pairs when creating new rsc using the dialog_new_rsc action.
* CONTRIBUTORS: added myself to CONTRIBUTORS file
* mod_admin:  action_admin_dialog_new_rsc fix

Maas-Maarten Zeeman (43):

* core: Fixes IE problems in zotonic js
* core: IE8 fixes for ubf js
* build: Added delete-deps
* core: Prune context and spawn notifier process only when needed
* core: Remove error and close handlers before ws restarts.
* build: Fix make clean
* core: Fix z_sites_dispatcher so it accepts empty paths. Fixes #842
* core: Added recon application
* mod_base: Removed commented out code.
* mod_seo: Make noindex and notrack configurable from templates
* Changed the websocket implementation.
* Removed comment to fix edoc generation
* mod_base: More careful websocket handshake.
* core: Fix: return default when there is no session.
* core: Export function to create a checksummed url for css and js lib files.
* mod_component: Added new module to make and use components on your page.
* fix for lazy loading css
* core: prevent reconnecting a ws when the page is unloading
* mod_base: Close websocket when unloading page. Fixes #898
* core: Fix a problem with mapping topic names to local names.
* Merge pull request #909 from witeman/emqtt_auth_issue
* mod_component: Fix bug in initilizing an already loaded component. Typo
* core: Make sure the z_file_entry fsm uses correct timeouts
* mod_component: delegate onunload to the component controller
* mod_component: updated mithril.js
* mod_component: Ignore load requests for components not in pending state
* z_utils: Allow trusted js values.
* mod_mqtt: Add an ack callback to subscribe calls.
* core: Fix for postback triggered by mqtt events.
* mod_admin_identity: Fix for adding email addresses
* core: Removed compile warning
* mod_component: Make it possible to use iolist init scripts
* core: Cache generated link and script tags when mod_development is disabled
* core: Do a session_context fold to get the right values in the context. Fixes language problem in websockets.
* core: Make acceptor_pool_size configurable. Fixes #923
* core: change default number of acceptors to 75.
* mod_acl_simple_roles: Fix to prevent blanking out all acl settings when role rsc is changed.
* mod_base_site: Make configuring the site's navbar logo easier
* mod_base_site: Minor base template fixes.
* filewatcher: fix, file_changed/2 moved
* mod_base_site: Fixed typo
* mod_base: Make synchronous ajax requests when the page is unloading
* mod_search: Make it possible to pass rsc_lists as parameter to hasanyobject.

Marc Worrell (280):

* smtp: Initialize e-mail server settings on startup. This is needed now that disc_copies are used.
* mod_survey: evaluate empty jump conditions to 'true'
* mod_menu: fix for passing the item_template option to the server when adding items.
* core: do not delete/insert edges when changing the order via mod_menu
* core: remove nested transaction from the z_edge_log_server check.
* mod_admin: if date is not editable, display it as text.
* mod_admin: more generic css for .category spans.
* mod_menu/mod_admin_frontend: enable editing of collections as side-menu.
* mod_menu: bootstrap3 change.
* core: add mime type exception for WMA files, they were recognized as video files.
* core: remove debug statement from m_media
* core: fix concatenating certain combined file streams.
* filestore: use filezcache:locate_monitor/1 to let the filezcache track z_file_entry processes. Fix difference in keys for uploading and * downloading. Better debug/info messages.
* mod_admin_identity: prevent Safari autofilling username/passwords in new user-account forms. Fixes #811
* translation: added some missing nl translations.
* erlydtl: allow lookup of var.key for a list [{<<key>>, ...}]
* core: m_rsc:p_no_acl/3 request for a non-existing resource should return 'undefined', just like m_rsc:p/3.
* core: For websocket, keep reqdata information for 'is_ssl' checks. When pruning for contexts, keep socket info for the is_ssl check.
* core: add sanitization of text/html-video-embed. Move #context handling out of z_html to z_sanitize.
* core: add acl mime check to m_media:replace/3.
* mod_admin: when replacing a media item, show the oembed/video-embed panels for embedded content.
* base: refactor the moreresults action.
* m_identity: don't crash if #identity_password_match{} doesn't match any observers.
* core: add lager warnings when modules are stopped.
* core: lager info with modules to be started.
* mod_tkvstore: don't start as named module.
* admin: force to select category when adding new content.
* menu: edge menu sorter has a problem with sorting nested collections. Disable sorting for now.
* core: module start/stop progress messages are now debug level.
* core: add m.modules.active.mod_foobar to test if a modules is active. Remove checks with the code server if a module is running.
* m.modules: replace usage of m.modules.info. with m.modules.active.
* core: on context prune-for-scomp, leave an empty list for request headers. Normalize user-agent lookup.
* core: fix args for transport ack.
* mod_email_receive: when adding recipiens, catch references to non existing rsc
* core: make session cookie name configurable (solves problems where old cookies might interfere, especially on Chrome)
* Merge pull request #864 from driebit/fix-menu-subtree-name
* Merge pull request #865 from driebit/remove-is-int-menu-subtree
* core: z_search: fix acl check sql query.
* mod_survey: allow printable overview if user has edit rights.
* mod_base: remove extra </div> from phone/_navbar
* mod_admin: remove api dispatch rule, also defined in mod_base.
* mod_base: for catinclude, don't assign passed categories to 'id'. Only assign a resource id to id.
* mod_menu: remove console.log message.
* core: allow a non-integer category id to be passed to all_flat/2
* mod_admin/mod_admin_frontend: preparations to allow creation of resources via the edit page.
* core: allow setting any rsc property that is 'undefined' to 'false'.
* core: ensure all db timestamp columns have a time zone.
* mod_menu: correct pubzub.publish topic.
* mod_admin_frontend: fix a problem where combining the nestedSortable.js lib with other js files will result in errornous drag behaviour
* mod_menu/admin_frontend: final fix for new-page topic on menu insertion.
* core: removed debug from z_pivot_rsc
* core: fix problem where the custom redirects form was not saved
* core: fix problem where mod_signed_url could not keep the user logged on.
* mod_mqtt: fix problem where removing one topic listener removed all listeners. Cleanup live subscriptions for removed elemnts.
* core: added comment explaining expire_1 and expire_n for sessions. Issue #881
* Merge pull request #880 from witeman/mod_authentication_logon_display_bugfix
* smtp: more relaxed error handling for spamd errors.
* install: use openssl to generate the admin password, as tr/urandom combo hangs on OS X.     Fixes #847
* mod_base: do not redirect if redirect id is set to undefined
* mod_base: in do_popupwindow use e.preventDefault() to play nice with multiple click event listeners.
* mod_mqtt: fix js error in for loops.
* core: the {% script %} tag has now arguments.
* mod_authentication: Refactor twitter/facebook logon and signup code.
* mod_facebook: add delegate for saving settings.
* mod_base: fix js error in livevalidation.
* mod_authentication: export send_reminder/2 and lookup_identities/2.
* Merge pull request #872 from driebit/auto-create-db
* docs: adapt docs to changes in files.
* core: fix specs in z_db.
* mod_oembed: remove http: protocol from embed html, this enables content to be viewable on https: pages.
* core: add exceptions for .xls and .xlsx files to z_media_identify. Fixes #893
* media/embed: fixes for twitter streaming, added notifications for importing and analyzing fetch url media-data.
* mod_search: allow dates like 'now' and '+2 weeks' in search questions.
* mod_admin: return the correct context in controller_admin_media_preview
* mod_translation: add more rtl languages.
* mod_twitter: fix edoc problem.
* doc: add mod_component.
* mod_oembed: don't display the media twice in the admin
* docs: added 0.12.2 release notes
* mod_oembed: add missing fallback _oembed_embeddable.tpl
* mod_oembed/mod_twitter: prefer our own 'website' extraction above oembed links. Pass the tweet url in the import_resource notification
* mod_twitter: fix import of tweets with special-html chars, was double escaped in title.
* mod_admin: always show media content, independent if size was defined.
* mod_oembed: sanitize received oembed code html and texts.
* core: add instagram js and urls to whitelist.
* mod_survey: fix problem where displaying the results did not work due to move sanitization functions.
* core: lock new z_stdlib library. Fix twerl git url. Fixes #895
* docs: fix length of header-underline
* docs: add 0.12.3 release notes
* Merge pull request #897 from driebit/connect-postgres-db
* core: refactor database creation on site init.
* mod_authentication: add authentication via LinkedIn. Add possibility to connect/disconnect accounts with FB/LinkedIn/Twitter.     Fix * redirects after using an external service for authentication.     List connected authentication services in the password reminder email.
* mod_linkedin: modify template for bootstrap3
* m_identity: fix spec of get_rsc_types/2
* mod_admin_identity: some extra padding for the identity verification page.
* mod_authentication: add optional page_logon for logon-title and an alert box on the logon page.
* mod_authentication: add special error message if there are cookie problems and the current browser is Safari 8.  Issue #902
* mod_signup: show external auth services for signup using the logon methods.     Also always force the presence of an username_pw identity for * signed up users.
* mod_linkedin: seems LinkedIn doesn't like URL encoded secrets?
* mod_admin: also log stacktrace on a catch.
* mod_oembed/mod_video_embed: fix problem with access rights if new media insert was done without admin rights.
* core: set the edge's creator_id on insert
* mod_survey: fix 'stop' survey button.
* core: fix stacktrace shown in transport lager messages.
* core: move erlang:get_stacktrace() outside of lager calls. Otherwise a stacktrace of lager will be shown due to the parse transforms.
* mod_twitter: Fix twitter redirect url
* mod_admin: add pubzub and some related javascripts. needed for live tags etc.
* mod_authentication/mod_twitter/etc: changes for new font-awesome, bs3 and some small tpl fixes
* mod_oembed: don't crash on oembed connect timeouts.
* mod_instagram: authenticate and import tags from Instagram
* core: fix problem where erlydtl_runtime crashed on fetching a value from a 'time_not_exists' atom.
* core: truncate the slug at 78 characters.
* core: fix in m_identity where fetching email identities could loop on a check if the email property was known as an identity.
* mod_base: handle ping/pong websocket control frames, remove name conflict with zotonic ping/pong.
* mod_linkedin: try to workaround a problem where LinkedIn doesn't recognize the Access Token it just handed out.
* mod_linkedin: work around for a problem with access-tokens at linkedin.
* core: allow binaries for some special keys.
* mod_import_csv: major changes to mod_import_csv.
* mod_instagram: fix property name in comment.
* mod_import_csv: added checks to the model creation.
* mod_base: check dialog height repeatingly, account for rounding errors in height calculation.
* core: add 'expected' option to m_rsc:update.
* mod_l10n: add utf-8 encoding hints to source file
* mod_l10n: adaptations for utf8 parsing changes in R17
* core: fix for importing structured blocks (like during imports)
* core: on startup z_dropbox moves now all processing files to unhandled.
* core: z_datetime:to_datetime/1 now also handles numerical timestamps.
* core: m_rsc:update now converts non-tuple dates and handles creator/modified on import correctly.
* mod_import_csv: fixes for file handling and medium_url imports.
* core: fix problem in m_rsc:update where modified was not set on save.
* mod_base: added the filter 'trans_filter_filled'
* core: remove unreachable code.
* docs: added placeholders.
* mod_import_csv: fix handling of blocks. Add support for 'blocks.name.field' keys in m_rsc:update
* core: add compile/0 and /1 to z.erl, for compiling without flush.
* docs: added xelatex target to generate PDF.
* mod_mqtt: allow topics like ['~site', 'rsc', 1234].
* mod_admin_identity: typo in translation.
* mod_admin_identity: publish identity changes to the topic ~/rsc/1234/identity.
* core: added e.issuu.com and static.issuu.com to the sanitizer whitelist.
* mod_import_csv/core: fixes for importing categories, new properties, corrected basename in #import_csv_definition{}
* doc: cleanup of pdf version.
* docs: add link to the pdf version.
* docs: better link text to the pdf version.
* docs: move the cookbooks to their own top level chapter.
* docs: correct the {% call %} documentation.
* skel: add mod_mqtt to the base site, as it is needed by mod_admin
* core: correct the language utf8 encoding for R16+
* mod_base: added filter trans_filter_filled/3 export.
* mod_admin: fix a problem where quick-editing a rsc adds all enabled languages.
* mod_base: filter-sort of undefined is undefined.
* core: correctly parse  multipart/signed emails.
* core: better handling of errornous urls for the z_file/media routines.
* core: extra utf8 sanitization of received email's subject, text, html and from.
* mod_content_groups: new module to categorize content into groups for the access control.
* m_hierarchy: merge m_menu_hierarchy and m_category into m_hierarchy. m_category now interfaces to m_hierarchy.
* Merge pull request #922 from imagency/master
* Merge pull request #921 from CyBeRoni/configure-dbcreation
* core: added support for 'is_dependent' flag.
* core: set longer timeout for renumber_pivot_task query.
* core: check if rsc existed before adding a rsc_gone entry.
* mod_admin_identity: correct the button class on the identity verification page.
* mod_mailinglist: more efficient query for polling scheduled mailings.
* mod_mqtt: allow 'live' wiring postbacks to mqtt topics, depending on the presence of an element-id.
* mod_email_status: new module to track email recipient status. Changes to the email bounce, sent and failure notifications.
* docs: fix length of header underline.
* docs: add mod_email_status placeholders.
* mod_email_status: fix problem with re-installing the model.
* mod_email_status: handle non-binary error status values.
* mow_acl_user_groups: first working version. To do: docs and test interface.
* mod_menu: breack cyclic dependency [mod_acl_user_groups,mod_authentication,mod_admin,mod_menu,mod_content_groups]
* Merge pull request #933 from driebit/fix-page-block-links
* Merge pull request #936 from CyBeRoni/override-pidfile
* mod_base: remove 'render-update' observer, leave template rendering to modules for more control
* mod_l10n: add some extra country-name to country-code mappings.
* mod_l10n: fix iso lookup of Serbia and Montenegro
* core: add log message when adding rsc.is_dependent and rsc.content_group_id.
* core: add is_meta/2 function to test if a category is a meta category, needed for determining the default content group in m_rsc:get/2
* mod_content_groups: use the m_category:is_meta/2 to determine the default content group.
* mod_content_groups: convert tabs to spaces.
* mod_acl_user_groups: fix search restriction if user can't see any content groups.
* core: fix progressbar for form posts with body.
* mod_content_groups: fix adding content_group_id for #rsc_get{} on an non-existing resource.
* mod_base: added the action update_iframe
* mod_email_status: add status dialog and simple templates to see the status of an email address.
* mod_base: missing part of the update_iframe addition
* mod_base: cross-platform fix for update_iframe
* mod_survey: add is_autostart option. Fix some minor template errors.
* core: use 'optional' includes for blocks.
* mod_survey: use the 'optional' include for the templates.
* core: normalize the name of the z_update_iframe JS function with its Erlang equivalent.
* mod_import_csv: Force convert to utf8 of the top row. Add test for ';' as a column separator.
* core: fix 'next' page number for queries returning a #search_result{}.
* mod_admin: use search query 'admin_overview_query' for the overview page if it is defined.
* mod_admin: add admin_content_query, also displayed in the content menu.
* mod_admin: enable 'all pages' button if a qquery is defined.
* core: ensure that the compiled template's name equals the one found via an (optional) catinclude. Issue #938
* mod_content_groups: 17.x compatibility for string with unicode character (breaks 15.x)
* mod_survey: fix crash on prep_answer if question name is not unique
* core: fix sql query in category delete.
* mod_acl_user_groups/mod_content_groups: ensure the hierarchy if the content/user groups are changed.
* mod_admin: in tree-list, show the category in gray.
* core: escape filenames using a single quot. Fixes #924
* core: shortcut for lib file lookup without filters (don't check the file store)
* core: add mod_mqtt to the default installed modules (as it is a dependency of mod_admin).
* core: fix m_category/2 lookup. Thanks to Alvaro Pagliari.
* Merge pull request #946 from AlainODea/BUGFIX_issue891
* core: fix problem where a gmail autoreply was classified as a bounce. Fix tracking of some email errors. Started collecting email test data.
* mod_email_status: change info message about previous problems.
* mod_admin: pass all strings from the new-rsc dialog form. Fixes #948
* mod_acl_user_groups: when changing category/content-group, show the meta category if the id is a meta or the current user is 1 (the admin)
* Merge branch 'master' into acl-content-groups
* zotonic_status: use default 'post' method for the logon form to prevent showing the password if there is a js error.
* mod_linkedin: adaptations for LinkedIn API changes.
* mod_linkedin: fix for picture-url.
* mod_search: fix a problem where a 'hassubject=[...]' query term was incorrectly parsed. Fixes #950
* mod_survey: change the thank you text, remove the 'Be sure to come back for other surveys' text.
* mod_search: add cat_exact query argument.
* mod_base: fix html_escape function in zotonic-1.0.js
* mod_admin_identity: on the users page, only show those with an username_pw entry. Issue #747
* mod_admin_identity: show 'email status' buttons if mod_email_status is enabled.
* mod_search: add 2nd ordering on -publication_start to featured search.
* mod_search: fix search_query for 'hasobject=123'. Fixes #953
* core: do not create atoms from rsc names in catinclude.
* core: add tick_10m and tick_6h notifications
* core: fix a problem where ImageMagick identified PDF files as PBM.
* core: cleanup location_lat/lng update/pivot code.
* mod_base: in scomp_lazy, ensure the 'visible' argument to all 'moreresults' actions
* Merge pull request #955 from pguyot/patch-2
* Merge pull request #956 from pguyot/patch-3
* mod_admin: pass all strings from the new-rsc dialog form. Fixes #948
* Merge pull request #958 from driebit/fix-signup-delegate
* core: adding some test data for smtp tests.
* Merge pull request #960 from trigeek38/fix-dialog-new-rsc
* Merge pull request #961 from trigeek38/fix-my-fix
* mod_admin_identity: fix user query.
* mod_admin: make action admin_dialog_new_rsc more robust against illegal objects arg
* mod_admin_identity: small changes/fixes to the users query.
* mod_acl_user_groups: add 'block' option to access control rules. Update the view via a 'live' subscribe.
* Start removing R15 support.
* core: move more code into the m_hierarchy update to prevent race conditions.
* core: in m_hierarchy, lock the rows of a hierarchy when updating.
* core: trace module (re-)loads and restart modules if any exported functions are changed. Issue #964
* core: bit more silent start/stop of modules.
* core: add realtime block list checks for incoming email. Needs updated z_stdlib.
* mod_development: move fswatch/inotify to the core.
* core: add cli commands 'zotonic startsite|stopsite|restartsite'. Fixes #964
* core: fix delete of timer in fswatch.
* core: log warnings if trying to insert duplicate rsc name/uri/path
* Fixes for language handling. Allow undefined pref_language for user. Filter on valid language code on pref_language upate. Show select list * with all valid languages in /admin/translation.
* core: add m_rsc 'is_linkable' lookup, also in z_acl and m_acl
* docs: add placeholders for ACL documentation.
* mod_search: allow search terms in texts without '=value' arg, map to '=true'. Fixes #970
* [docs] proposal for site-fsm
* [core] add possibility to fetch sub-trees from a hierarchy. Example: m.hierarchy.content_group[id].tree1
* [docs] A websocket connection is opened by the browser, accepted by the server.
* mod_menu: fix issue that on insertion of a new item it is added to all sub-menus. Fixes #971
* mod_search: add query term 'hasanyobject', to search using an 'or' on outgoing edges. Fixes #968
* mod_acl_user_groups: new import/export version of the acl rules. Include the content groups, user groups and hierarchies in the export.
* mod_acl_user_groups: move ensure_name to the core m_rsc module.
* mod_menu: in the menu-editor, add open/close buttons for submenus. This makes editing big lists easier.
* docs: fix documentation of smtp server settings. Also fix z_config settings.
* core: name correction, the bounce server is a complete smtp receive server.
* docs: clarification of the bounce addresses.
* core: allow pivot task to return {delay, DateTime}.
* core: fix a problem where m_rsc:ensure_name/2 could insert duplicate names. Fixes #974
* core: add reserved usernames to prevent users signing up as these. Fixes #929
* core: if some reserved username was taken, allow to update the password. Issue #929
* core: allow diff between Date and DateTime.
* core: add 'flickrit.com' to the iframe whitelist.
* core: copy tmpfiles if they are mailed, add periodic cleanup of the tmp folder. Fixes #972
* core: fix a problem where a fulltext search with an empty category list didn't return any results. Fixes #976
* core: fix mqtt login, don't start a session_page if there is no session or reqdata. Fixes #973
* mod_admin: fix problem where the lazy-loader for moreresulsts kept loading till no results where left.
* Set version number to 0.13.0
* Lock deps
* docs: 0.13.0 release notes and some extra (minimal) documentation.
* docs: add tentatve 0.13.0 release date
* core: determine mime type of attachments if it was not given.
* core: use 'rebar' to compile zotonic from the command line. Move .yrl output to default location for rebar. > Use skip_deps for 'make compile-zotonic'. Add 'make refresh-deps' and 'make list-deps'. Fixes #978

Marco Wessel (4):

* Allow configuration of db creation and installation
* Add commented dbinstall/dbcreate options + explanation
* Add warning message that db won't be created or installed
* Allow to override pidfile location with env var

Paul Guyot (3):

* Fix typo in _block_view_survey_thurstone.tpl
* Handle &# entities when splitting markers
* mod_survey: Fix multi & single HTML structure for thurstone checkboxes

Paul Monson (1):

* Update README link

Sergei (2):

* fix dependencies build order
* force rebar to build 'setup' app

Witeman Zheng (1):

* mod_authentication: fix logon_box form input "password"

imagency (12):

* Tinymce support for Russian

肥仔 (2):

* emqtt_auth_zotonic issue would cause crashed when mqtt client try to connect onto it.
* Fix the emqtt client connection issue.





