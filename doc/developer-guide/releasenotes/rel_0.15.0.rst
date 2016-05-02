.. _rel-0.15.0:

Release 0.15.0
==============

Welcome to Zotonic 0.15.0, released on 4 April, 2016.

Main changes are:

* Added ``is_number`` template filter.
* Added ``persistent_set`` template action.
  resource.
* Added mod_media_exif.
* Improved mod_export.
* Upgraded TinyMCE to 4.3.7.
* Fixed :issue:`1216` by changing ``lossless`` media option to ``false`` (instead
  of ``auto``).
* Fixed :issue:`1148` by adding 403 page for logged in users that have no access
  to the current
* Fixed :issue:`1205`: DPI for resized images.
* Fixed :issue:`1224`: limit access to mod_rest and mod_export to users that
  have ``use`` permission.
* Fixed :issue:`1221`: order of Growl messages.

Commits since 0.14.0
--------------------

There were 60 commits since release 0.14.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arthur Clemens (3):
      * mod_admin: remove extraneous closing div
      * mod_admin: move footer to its own row
      * mod_admin: add a bit of space to the footer

David de Boer (4):
      * mod_admin: Fix link to search docs (#1220)
      * doc: Fix ACL rule fixture syntax
      * scripts: Fix comment
      * Prepare 0.15.0

Marc Worrell (53):
      * core: fix a problem where the comet loop can crash if the page process dies.
      * mod_base: add 'is_number' filter.
      * mod_backup: move the list of known resource properties to m_rsc.
      * mod_export: refactor export, separate encoder functions. Added xlsx export encoder.
      * mod_survey: remove controller_survey_results, mod_export is now used.
      * docs: remove controller_survey_result docs.
      * docs: add documentation for filter is_number
      * mod_export: add vcalendar export (ics)
      * mod_export: connect mod_export to the 303 handling of controller_id
      * mod_export: remove #export_resource_data observe, this is already handled in export_encoder:do_body/2
      * mod_base: new action 'persistent_set'
      * docs: fix inline quote error.
      * docs: add persistent_set to the doc tree
      * core: also look into ~/.zotonic/<major-version>/.. directory for configuration.
      * mod_export: filter tags and unescape html with spreadsheet exports. Add 'raw' option to not filter/unescape.
      * mod_export: fix double reverse of xlsx rows
      * mod_survey: also show the prompts in the answers exports.
      * core: use Erlang 'exif' module and add mod_media_exif to extract resource properties (gps, orientation, crop, date)
      * Run travis on 0.x
      * docs: add simple documenttaion for mod_media_exif
      * core: add mod_media_exif to the core modules.
      * core: fix for focus point calculation.
      * tinymce: add version 4.3.7. Add option to add captions to the body media.     Also:      * Add a template for the image options dialog      * Move some named wires to _editor.tpl to prevent multiple initialization for every single editable body.
      * mod_editor_tinymce: remove version 4.0.26 and 4.1.6
      * mod_editor_tinymce: remove the deleted tinymce versions from the configure dialog
      * mod_editor_tinymce: include the correct css version
      * mod_export: simplified download buttons for the admin sidebar
      * mod_export: add explanation for event download
      * mod_authentication: add 403 page with logon form (or redirect button for secure page)
      * Switch to nlfiedler/erlang-exif.git instead of our own branch
      * Switch to nlfiedler/erlang-exif.git instead of our own branch
      * mod_base: remove comet streamhost from zotonic js
      * core: remove mentions of streamhost (which is unsupported)
      * New mochiweb
      * mod_filestore: remove GreenQloud - they transferred their business to another company.
      * core: start using psql 'IN (SELECT(unnest(::int[])))' instead of concatenated id strings.
      * mod_twitter: fix a problem where httpc sessions were not closed.
      * New twerl library
      * mod_base: restart ws/comet if navigating back to the page in iOS/Safari
      * mod_base: tune stream restart on pageshow of persisted pages.
      * mod_base: better session check on pageshow. Still a problem if tinymce is enabled and the page is re-visited for the 2nd time (twice back to the page).
      * mod_mailinglist: add some useful shortcuts to the edit sidebar panel
      * New tw* erl dep
      * core: ensure that resized images have a density of 72DPI. Fixes #1205
      * Fix media preview test for dpi forcing
      * mod_base: show newer growl messages on top. Fixes #1221
      * core: change media preview option 'lossless' default to 'false' (instead of 'auto'). Fixes #1216
      * Fix a problem with filtering on content-group in searches.
      * mod_acl_user_groups: fix a problem where the ACL tree expand could not find some entries.
      * mod_acl_user_groups: fix problem adding new rules.     Stabilize the order of rules by including the rule creation date and id into the sort order     Split system content groups in pull-down, to clearify that 'all' doesn't apply to the system content groups.
      * mod_export: limit exports to users with mod_export.use permission. Refactor export api, simple privacy filter for email address. Issue #1224
      * mod_rest: add acl check for mod_rest.use. Issue #1224
      * mod_logging: fix a problem with filtering on content-id and other-id.
