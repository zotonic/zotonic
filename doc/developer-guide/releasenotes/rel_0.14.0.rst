.. _rel-0.14.0:

Release 0.14.0
==============

Welcome to Zotonic 0.14.0, released on 7 March, 2016.

Main changes are:

* From now on, we follow Semantic Versioning by increasing
  `the minor instead of the patch number <https://github.com/zotonic/zotonic/issues/1204>`_
  for our monthly releases.
* Added `data model notification <http://zotonic.com/docs/latest/developer-guide/modules.html#data-model-notification>`_.
* Added `managed ACL rules <http://zotonic.com/docs/latest/ref/modules/mod_acl_user_groups.html#managing-acl-rules-in-code>`_.
* Added ``zotonic wait`` and ``zotonic rpc``
  `commands <http://zotonic.com/docs/latest/ref/cli/index.html>`_.
* Added new user category
  `configuration parameter <http://zotonic.com/docs/latest/ref/modules/mod_admin_identity.html#configure-new-user-category>`_.
* Fixed redirect to login when user has no permissions by showing 403 page with
  login form.
* Fixed several import and export bugs.
* Fixed Dutch translations for mod_acl_user_groups.
* Fixed status page login button.
* Improved documentation.
* Improved m.req model.

Commits since 0.13.8
--------------------

There were 65 commits since release 0.13.8.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (17):
      * mod_import_csv: Fix CSV import for CR files
      * Add release preparation script
      * Restructure documentation into guides and reference
      * Add data model notification
      * Increase resource URI length
      * Move 0.13.7 release notes to new dir
      * doc: Document how to build docs
      * mod_admin_identity: Add config param for new user category
      * Support managed ACL rules
      * mod_acl_user_groups: Improve wording and add missing Dutch translations
      * Prepare hotfix release 0.13.8
      * doc: Improve access control chapter
      * Move release notes to proper location
      * Prepare release 0.14.0
      * doc: Fix link
      * doc: fix notification reference
      * Add 0.14.0 release notes

Lil' Dash (1):
      * Fix the leap year error(issue : #1202)

Maas-Maarten Zeeman (15):
      * core: Added reference to active comet connection. Preparation for connection test page
      * mod_base: Added connection test page
      * core: Return 0 bytes when there is nothing to transport. Related to #1097
      * core: Typo in refactored controller_comet
      * mod_base: removed debug statement from comet controller
      * admin: Make it possible to brand the admin console
      * mod_base: Removed delegate macro, it was tripping edoc generation
      * core: Fixes setting and resetting ws transport timeouts. Related to #1116
      * mod_base: Let postback and comet return text/x-ubf mimetype.
      * build: Make it possible to separate user beams
      * build: Add user_ebin_dir to zotonic.config.in
      * build: Fix for the case when there is no user work
      * doc: Repair underline warning.
      * mod_mqtt: Restart middleman subscriber processes. Fixes #1201
      * mod_ssl: Create and use a file with dh parameters. Fixes #1198

Marc Worrell (31):
      * mod_export: encode YMD only date as UTC, add fallback for other dates to handle illegal dates.
      * mod_export: suppress ST_JUTTEMIS and year 9999 dates in the output.
      * Lock new depcache. Fixes a problem where the wrong depcache could be used for lookups. Fixes #1172
      * core: refactor transport of multipart posts and comet polls.
      * core: fix specs of refactored z_parse_multipart functions.
      * mod_base: only initialize callback forms after javascript delegate calls, not after other delegates. Issue #1159
      * core: allow 'none' as crop argument.
      * core: allow flexible 'visible_for' settings.
      * docs: add doc for m.category.text.tree_flat
      * core: use the 'image' dispatch rule to generate image urls.
      * core: add more information and better docs for the m.req model.
      * core: (m_rsc) allow template access to page_url and other 'public' properties of non accessible resources.
      * core: show 403 page with option to login instead of redirecting to login controller. Fixes #1148
      * core: fix 'provide_empty' in http error controller.
      * core: fix erlydtl debug for binary filenames.
      * mod_twitter: drop tweet id 0
      * mod_translation: add interface to change the i18n.language_stemmer configuration
      * mod_oembed/mod_video_embed: add the medium properties video_embed_service and video_embed_id. Fixes #941
      * mod_base: added actions overlay_open/overlay_close
      * mod_backup: fix acl check for revisions
      * core: fix redirect returns within dispatcher for alternate hosts.
      * mod_acl_user_groups: fix a problem with edge checks for hasusergroup edges. Fixes #1199
      * mod_authentication: fix ru translation for 'Sign in' Fixes #1197
      * core: allow '{lossless, false}' in mediaclass definitions.
      * core: fixes for initialization and startup of new sites.
      * mod_acl_user_groups: fix schema initialization. Fixes #1200
      * core: fix for check of db schema exists.
      * core: better check query for schema existance.
      * core: filter the watch_dirs on dead links, this fixes a problem with inotify stopping on removed link targets
      * core: fix a problem where the comet loop can crash if the page process dies.
      * mod_survey: do not crash on unknown survey question types when preparing charts.

Marco Wessel (1):
      * scripts: Add zotonic-rpc and zotonic-wait commands
