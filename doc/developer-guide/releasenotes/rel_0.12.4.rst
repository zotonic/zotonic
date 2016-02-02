Release 0.12.4
==============

Released on 2015-02-20 14:19 by arjan.


Arjan Scherpenisse (13):

* core: Add 'preview_url' to rsc export API call
* core: Fix edocs build
* doc: Clarify that finished/upcoming filters don't perform sorting
* mod_acl_simple_roles: Fix ACL check when category of rsc cannot be found
* mod_development: Enable automatic recompilation on MacOS X
* mod_search: API: Add limit, offset, format arguments to search API
* mod_search: Add 'finished' search filter + documentation
* mod_search: Add {finished} search method
* mod_survey: Add 'submit' API service
* mod_twitter: Close login window when user denies login request
* mod_twitter: Set all context vars when rendering logon_done template
* mod_twitter: Show logon page in current context's language
* mod_video: Make command line to ffmpeg/ffprobe calls configurable

Arthur Clemens (44):

* mod_admin: remove confusing text
* Social login: use FB compliant "Login with..."
* admin: fix zlink from editor, remove superseded template
* admin: include font-awesome version 4
* admin: tidy up Authentication Services page
* basesite: remove old fix for logo image
* doc: Document template models
* doc: add icons documentation
* doc: document 'page_url with' syntax
* doc: document more use cases for ``|if``
* doc: formatting
* doc: remove deprecated %stream%
* doc: restructure actions
* doc: restructure filters
* doc: reword solution
* doc: show css output when extending icons
* mod_admin: cosmetic fixes logon widget
* mod_admin: fix layout person form
* mod_admin: include icons css instead of recreating with less
* mod_admin: layout tweaks
* mod_admin: mobile tweaks
* mod_admin: tidy email table
* mod_base: add non-circle icons
* mod_base: add share icon
* mod_base: add social login icons
* mod_base: clean up documentation
* mod_base: hide original x char
* mod_base: include all font source files
* mod_base: make Z icons independent of FontAwesome
* mod_base: make icon extend cleaner
* mod_base_site: add icon css
* mod_base_site: better defaults
* mod_bootstrap: version 3.3.2
* mod_development: flush when translation file changes
* mod_facebook: typo
* mod_filestore: tidy up html
* mod_filestore: use 0 if archive size is undefined
* social login: use icons and update brand colors
* translation tweaks (NL)
* validation: provide message_after param to templates

David de Boer (1):

* Connect to "postgres" when creating database

Maas-Maarten Zeeman (4):

* core: Make sure the z_file_entry fsm uses correct timeouts
* core: prevent reconnecting a ws when the page is unloading
* mod_admin_identity: Fix for adding email addresses
* mod_base: Close websocket when unloading page. Fixes #898

Marc Worrell (67):

* New z_stdlib locked.
* core: add 'expected' option to m_rsc:update.
* core: add compile/0 and /1 to z.erl, for compiling without flush.
* core: added e.issuu.com and static.issuu.com to the sanitizer whitelist.
* core: allow binaries for some special keys.
* core: better handling of errornous urls for the z_file/media routines.
* core: correct the language utf8 encoding for R16+
* core: correctly parse  multipart/signed emails.
* core: extra utf8 sanitization of received email's subject, text, html and from.
* core: fix for importing structured blocks (like during imports)
* core: fix in m_identity where fetching email identities could loop on a check if the email property was known as an identity.
* core: fix problem in m_rsc:update where modified was not set on save.
* core: fix problem where erlydtl_runtime crashed on fetching a value from a 'time_not_exists' atom.
* core: fix stacktrace shown in transport lager messages.
* core: m_rsc:update now converts non-tuple dates and handles creator/modified on import correctly.
* core: move erlang:get_stacktrace() outside of lager calls. Otherwise a stacktrace of lager will be shown due to the parse transforms.
* core: on startup z_dropbox moves now all processing files to unhandled.
* core: refactor database creation on site init.
* core: remove unreachable code.
* core: set the edge's creator_id on insert
* core: truncate the slug at 78 characters.
* core: z_datetime:to_datetime/1 now also handles numerical timestamps.
* docs: add link to the pdf version.
* docs: added placeholders.
* docs: correct the {% call %} documentation.
* m_identity: fix spec of get_rsc_types/2
* mod_admin: add pubzub and some related javascripts. needed for live tags etc.
* mod_admin: also log stacktrace on a catch.
* mod_admin: fix a problem where quick-editing a rsc adds all enabled languages.
* mod_admin_identity: publish identity changes to the topic ~/rsc/1234/identity.
* mod_admin_identity: some extra padding for the identity verification page.
* mod_admin_identity: typo in translation.
* mod_authentication/mod_twitter/etc: changes for new font-awesome, bs3 and some small tpl fixes
* mod_authentication: add authentication via LinkedIn. Add possibility to connect/disconnect accounts with FB/LinkedIn/Twitter. Fix redirects after using an external service for authentication. List connected authentication services in the password reminder email.
* mod_authentication: add special error message if there are cookie problems and the current browser is Safari 8.  Issue #902
* mod_authentication: make logon form responsive, add optional page_logon with title/body texts.
* mod_base: added filter trans_filter_filled/3 export.
* mod_base: added the filter 'trans_filter_filled'
* mod_base: check dialog height repeatingly, account for rounding errors in height calculation.
* mod_base: filter-sort of undefined is undefined.
* mod_base: handle ping/pong websocket control frames, remove name conflict with zotonic ping/pong.
* mod_import_csv/core: fixes for importing categories, new properties, corrected basename in #import_csv_definition{}
* mod_import_csv: added checks to the model creation.
* mod_import_csv: fix handling of blocks. Add support for 'blocks.name.field' keys in m_rsc:update
* mod_import_csv: fixes for file handling and medium_url imports.
* mod_import_csv: major changes to mod_import_csv.
* mod_instagram: authenticate and import tags from Instagram
* mod_instagram: fix property name in comment.
* mod_l10n: adaptations for utf8 parsing changes in R17
* mod_l10n: add utf-8 encoding hints to source file
* mod_linkedin: modify template for bootstrap3
* mod_linkedin: seems LinkedIn doesn't like URL encoded secrets?
* mod_linkedin: try to workaround a problem where LinkedIn doesn't recognize the Access Token it just handed out.
* mod_linkedin: work around for a problem with access-tokens at linkedin.
* mod_mqtt: allow topics like ['~site', 'rsc', 1234].
* mod_oembed/mod_video_embed: fix problem with access rights if new media insert was done without admin rights.
* mod_oembed: don't crash on oembed connect timeouts.
* mod_signup: show external auth services for signup using the logon methods. Also always force the presence of an username_pw identity for signed up users.
* mod_survey: fix 'stop' survey button.
* mod_twitter: Fix twitter redirect url
* rebar.config.lock: lock new z_stdlib
* rebar.config.lock: new s3filez
* rebar.config.lock: new z_stdlib
* rebar.config.lock: new z_stdlib
* rebar.config.lock: new z_stdlib
* rebar.config.lock: new z_stdlib
* skel: add mod_mqtt to the base site, as it is needed by mod_admin

肥仔 (2):

* emqtt_auth_zotonic issue would cause crashed when mqtt client try to connect onto it.
* Fix the emqtt client connection issue.

