Release 0.9.5
=============

Released on 2014-04-18 16:55 by arjan.


Arjan Scherpenisse (20):

* mod_admin: Remove warning in m_admin_blocks
* doc: Document how to set m_config values in the site's config
* mod_mailinglist: Fix button order of delete confirmation dialog.
* mod_base: Match the websocket protocol on the WS handshake.
* z_stdlib: new submodule
* New eiconv submodule
* mod_survey: in printable overview, use fallback to column name when no column caption has been set.
* mod_survey: Add m_survey:get_questions/2 function
* mod_base: controller_page - check if page path equals the request path
* mod_base: controller_page - make canonical redirect behavior configurable.
* doc: Clarify the doc page about custom pivots
* core: basesite skeleton: install mod_acl_adminonly before mod_authentication
* core: z_utils:json_escape now points to right function in z_json
* core: Update dispatch info on start and stop of a site.
* core: Show error message in admin when a module fails to start
* doc: Update mod_twitter to show how to do Twitter login
* doc: Explain what a `query resource` is
* doc: Explain all default fields in a resource
* mod_mailinglist: Do not crash when recipientdetails filter encounters an empty body

Cillian de Róiste (1):

* doc: Add cookbook item on creating custom content blocks

Grzegorz Junka (1):

* core: Fix in Zotonic after removing access to the webmachine_request:send_response/2 function

Maas-Maarten Zeeman (4):

* core: Make sure js boolean values are converted correctly.
* mod_base: Make sure no messages are left in the request process's mailbox. Fixes #676
* mod_base: Limit the number of websocket reconnects. Fixes #675
* core: Prevent to detach comet controllers from page sessions which are already stopped.

Marc Worrell (19):

* core: periodic process to unlink deleted medium files (and their generated previews)
* z_stdlib: fixes for escape/escape_check of proplists.
* deps: new z_stdlib
* mod_import_csv: improved parsing of CSV files, now handles quoted fields and performs automatic translation from latin-1 to utf-8
* mod_import_csv: remove stray underscore character from module name...
* mod_import_csv: remove superfluous lager messages about escaped chars.
* mod_base: reapply change from d5604eca1f83491c37adf7981ff1cc598b7c57e2
* Fix on fix. start_timer returns a Ref, not {ok, Ref}.
* mod_base: 0.9.x doesn't know about z_ua.
* core: use lower case extensions when guessing mime or file extension.
* core: for exif lookups, add LANG=en for sensible exif program output (not for win32 as shell var is used))
* mod_base: use window.location.host for websocket stream, unless configured otherwise. Also add try/catch around websocket connect, to catch potential IE10 security exception.
* core: new state machine for maintaining websocket/comet push connections. Perpetual comet loop which only polls if there is no active websocket connection. Websocket connection checks with a periodic ping/pong if the connection works.
* core: fix difference between master and 0.9 regarding z_stream_start.
* mod_base: fix for subdomain polling loop.
* New gen_smtp from zotonic/gen_smtp/master
* mod_admin_identity: fix identity insert race condition by moving email identity check into the #rsc_update event instead of the async #rsc_update_done event.
* base: fix NS_ERROR_XPC_BAD_CONVERT_JS on Firefox. Fixes #718
* core: fix SVG mime type mapping from ImageMagick identify.

