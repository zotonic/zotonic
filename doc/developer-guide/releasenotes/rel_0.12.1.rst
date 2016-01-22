Release 0.12.1
==============

Released on 2014-10-20 17:01 by arjan.


Arjan Scherpenisse (6):

* core: Disable sendfile support by default and make a note of this
* core: Fix crashing make on initial build
* core: Use rebar locked on 0.12 as well
* core: When using locked deps, still add the deps from zotonic.config
* doc: Fix preinstall notes about buggy erlang versions
* scripts: Add "-v" argument to zotonic command to print the version

Arthur Clemens (1):

* doc: update references to priv/config

Maas-Maarten Zeeman (8):

* build: Added delete-deps
* build: Fix make clean command
* core: Fix z_sites_dispatcher so it accepts empty paths. Fixes #842
* core: Fixes IE problems in zotonic js
* core: IE8 fixes for ubf js
* core: Prune context and spawn notifier process only when needed
* core: Remove error and close handlers before ws restarts.

Marc Worrell (31):

* admin: force to select category when adding new content.
* base: refactor the moreresults action.
* core: For websocket, keep reqdata information for 'is_ssl' checks. 
* core: add acl mime check to m_media:replace/3.
* core: add lager warnings when modules are stopped.
* core: add m.modules.active.mod_foobar to test if a modules is active. Major performance increase.
* core: add mime type exception for WMA files, they were recognized as video files.
* core: add sanitization of text/html-video-embed. Move #context handling out of z_html to z_sanitize.
* core: do not delete/insert edges when changing the order via mod_menu
* core: fix concatenating certain combined file streams.
* core: lager info with modules to be started.
* core: m_rsc:p_no_acl/3 request for a non-existing resource should return 'undefined', just like m_rsc:p/3.
* core: module start/stop progress messages are now debug level.
* core: remove debug statement from m_media
* core: remove nested transaction from the z_edge_log_server check.
* erlydtl: allow lookup of var.key for a list [{<<key>>, ...}]
* filestore: use filezcache:locate_monitor/1 to let the filezcache track z_file_entry processes.
* m.modules: replace usage of m.modules.info. with m.modules.active.
* m_identity: don't crash if #identity_password_match{} doesn't match any observers.
* menu: edge menu sorter has a problem with sorting nested collections. Disable sorting for now.
* mod_admin: if date is not editable, display it as text.
* mod_admin: more generic css for .category spans.
* mod_admin: when replacing a media item, show the oembed/video-embed panels for embedded content.
* mod_admin_identity: prevent Safari autofilling username/passwords in new user-account forms. Fixes #811
* mod_menu/mod_admin_frontend: enable editing of collections as side-menu.
* mod_menu: bootstrap3 change.
* mod_menu: fix for passing the item_template option to the server when adding items.
* mod_survey: evaluate empty jump conditions to 'true'
* mod_tkvstore: don't start as named module.
* smtp: Initialize e-mail server settings on startup. This is needed now that disc_copies are used.
* translation: added some missing nl translations.

