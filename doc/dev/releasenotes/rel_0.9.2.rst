Release 0.9.2
=============

Released on 2013-07-02 20:53 by arjan.


Andreas Stenius (7):

* zotonic-tpl-mode: optimize font-lock regexp.
* zotonic-tpl-mode: optimize font-lock regexp #2.
* zotonic-tpl-mode: refactor tag soup indent calculations.
* zotonic-tpl-mode: autocomplete closing tag soup tags.
* zotonic-tpl-mode: improved auto close tag.
* zotonic-tpl-mode: fix bug in find tag soup open tag when encountering a self closed tag.
* doc: minor tweaks to the dispatch documentation.

Arjan Scherpenisse (19):

* mod_logging: Fix live display of log messages
* mod_base: Fix filter_capfirst to support i18n strings
* core: Add 'default_colorspace' global option to set the default ImageMagick colorspace
* mod_import_csv: Correctly treat .csv files as UTF-8
* Fix z_sites_dispatcher:split_host/2 for 'none' value
* zotonic_status: Add API for global site status check
* doc: Update preinstall instructions for Ubuntu
* mod_mailinglist: New dialog which lets you combine lists
* mod_base: Fix controller_static_pages's rendering of template files
* mod_mailinglist: Make bounce dialog nicer
* mod_mailinglist: Only show addresses in bounce dialog for the current mailing page
* mod_backup: Also prefix the periodic backups with the site name
* mod_admin: Resources in the meta category can have 'features'
* core: Fix startup script distributed detection
* core: eiconv should do a make clean, not a rebar clean
* core: zotonic CLI command now refers to website for help on subcommands
* code: Add m_edge.id[123].predicate[456] syntax to look up an edge id from a triple
* mod_acl_simple_roles: Fix bug where visible_for would be reset when it was not part of a rsc update call.
* mod_admin_identity: Add option to send username/password to the user over e-mail

Bryan Stenson (1):

* fix typo in default data

Marc Worrell (67):

* core: show info messages when a site starts/stops.
* core: erlydtl - allow lookup in a list of  n-tuples (n>2) by first tuple element.
* core: i18n - don't replace assume an empty translation should be added as the original lookup string. This fixes an issue where an undefined translation overwrites an existing translation from another module.
* core: remove accidently added notes about federated zotonic.
* core: i18n - removed warning
* docs: fix build on OS X (BSD sed is a bit different than on Linux)
* docs: simpler echo to file, remove temp file.
* docs: document #foo.id syntax
* core: added support for signing up with facebook for a known user. Moved some check functions to m_identity. Fixed problem where an email identity could be added multiple times (when signing up using FB)
* Fix for close button on persistent notices.
* mod_signup: fix spec
* mod_signup: fix typespec
* mod_facebook: fixes to redirects etc.
* mod_base: added 'only_text' option to alert. Corrected documentation.
* mod_authentication: remove reload of other open 'tabs' on log on/off. This gives a race condition in Chrome as Chrome doesn't close the Websockets channel during redirects (which are setting the new cookies)
* core: added z:log_level(debug | info | warning | error) to set the console backend log level.
* mod_development: show debug message when reloading a module.
* mod_base: cherry pick from 5d252cb. Issue #491
* mod_base: remove part of merge.
* mod_l10n: japanese translation for days and months.
* mod_base_site: remove scrollbars from share dialog
* core: don't crash when an identity has duplicates.
* mod_base_site: separate copyright template. Use tablet/desktop footer for (smart)phone.
* mochiweb: merged fix for missing summer time hour.
* mod_oembed: fixes for provider list. Added oembed_client:providers/2 call for debugging. Added lager logging.
* mod_import_csv: connected reset checkbox, now also forces re-import of items. Added support for 'medium_url' column name, downloads and imports an url for a medium item. Added lager logging. Added filtering of escaped quotes.
* core: added extra arg to m_media:replace/insert for m_rsc_update options.
* core: fix for upload from url.
* mod_base: simpler and better z.clickable.js
* mod_custom_redirect: merge of 0f8d234 from master
* mod_custom_redirect: fix delete buttons, and type of submit button
* mod_custom_redirect: check for access permission when saving configuration.
* core: fix for mimetypes where 'image/jpeg' returns preferred extension '.jpe' (instead of '.jpg')
* doc: added documentation about the site.bounce_email_override config.
* Fix for checking delivery status - header decode was using wrong function.
* core: smtp: use os:timestamp() instead of now(). Added guards and more strict tests for inc_timestamp/sent handling
* core: the hostname can be 'null', handle it as 'undefined'
* core: make more robust against illegal page paths from hacking attempts.
* mod_l10n: Added South Sudan. Needs additional translations
* mod_authentication: make the password reset available when logged on
* mod_authentication: fixed reminder link in error message.
* mod_base: fixes for wired id css selector support in combination with a postback. This hack should be replaced with proper 'selector' support for wired targets.
* core: don't change the language on internal page_path rewrites. Fixes #559
* smtp: fix e-mail receive.
* deps: new z_stdlib.
* mod_admin/mod_admin_frontend: made admin more responsive. Added first version of frontend editor based on mod_admin.
* mod_admin: fix '&' in doc description as it crashed the doc generation.
* core: fix failed merge.
* mod_base: copy if_undefined filter from master.
* mod_admin: fix body top padding for smaller screens.
* mod_admin: Make the modal popup in the admin_frontend as wide as in the admin.
* mod_menu: use short title; absolute positioning of item buttons. mod_admin_frontend: minor textual changes, add dutch translation
* mod_menu: show warning when a resource is unpublished.
* mod_admin/mod_menu: added option so pass tab to connect/new dialog.
* mod_admin_frontend: set default connect tab to 'new'
* mod_admin_frontend: preselect the 'text' category for page linking.
* mod_admin: make the logon more responsive (header row-fluid; some styling)
* mod_admin_frontend: fixes for editing blocks and language selections.
* mod_admin: nicer fixed save button bar
* mod_admin_frontend: see images in the tinymce. Nicer save/cancel bar
* mod_menu: small textual change
* mod_admin: fix tinymce image preview.
* mod_admin_frontend: remove debug javascript.
* mod_base/mod_translation: add filter 'filter'; added m.translation model for easy language access.
* mod_ssl: change behaviour of 'is_secure' setting; redirect to https when not specified otherwise.
* mod_ssl: added an 'is_ssl' configuration to force a site to be served over ssl. Use in combination with 'is_secure'.
* Merge pull request #579 from fenek/install-timeout-fix

