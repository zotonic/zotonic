.. _rel-0.13.3:

Release 0.13.3
==============

Welcome Zotonic 0.13.3, released on September 9, 2015.

This is a maintenance release of Zotonic 0.13

Main changes are:

 * refinements to the admin interface by Arthur Clemens
 * included Bootstrap version has been upgraded to version 3.3.5
 * fixes for some issues around the newly introduced content groups
 * CORS support for the API (Thanks to @ghosthamlet!)
 * fixes some issues with the websocket connection
 * fixes an issue where the wrong language could be selected


Commits since 0.13.2
--------------------

There were 103 commits since release 0.13.2.

Big thanks to all people contributing to Zotonic!


Git shortlog
............

Arjan Scherpenisse (1):

    *  mod_base: Support CORS API requests

Arthur Clemens (76):

    *  mod_admin: (small screen) prevent scrolling of body when menu open
    *  mod_admin: fix lost tree-list styling
    *  mod_admin: style tweaks
    *  mod_admin: position help buttons in widget headers
    *  mod_base: remove redundant div container in modal body
    *  Remove inadvertently created files
    *  mod_admin: create user dropdown in main nav
    *  mod_admin: improve sorting of page overview
    *  mod_admin: optional category-specific column
    *  mod_admin: update admin bootstrap
    *  mod_admin: prevent executing content_group query
    *  doc: Add dev note on translations; add batch update command
    *  mod_translation: Localize messages
    *  mod_base: make pager numbering more logical
    *  mod_base: update translation files
    *  mod_bootstrap: update to 3.3.5
    *  mod_base: move pager css overrides to mod_admin
    *  mod_admin: reorganize edit page header
    *  mod_admin: Make topbar elements fit on a smaller screen when country and account buttons are visible
    *  docs: release notes in reverse chronological order
    *  mod_editor_tinymce: keep text inside moved blocks
    *  mod_editor_tinymce: add translation files
    *  mod_editor_tinymce: Add latest TinyMCE 4.2.4, and updated codemirror plugin
    *  mod_admin: reorganize page edit header: keep image reference after saving
    *  mod_admin: optional category-specific column
    *  mod_admin: Fix unlinking media on Enter
    *  docs: Mobile friendly layout
    *  docs: Mobile friendly layout: fix search box layout
    *  mod_admin: revert "also check in .css files resulting from .less compilation"
    *  File watcher: read optional config file to run lessc with params and compile just the main less file
    *  mod_admin: reorganize page edit header: hide "by" if modified/creator does not exist
    *  mod_acl_user_groups: minor frontend tweaks
    *  mod_base: minor icon font tweaks
    *  mod_base: minor icon font tweaks
    *  Admin: straighten admin-header blocks
    *  Admin: add breadcrumbs to hierarchical pages
    *  File watcher: read optional config file to run lessc with params and compile just the main less file
    *  mod_translation: tidy up language settings dialog
    *  mod_admin: give logo a width to prevent jumping navbar
    *  docs: Add note about /etc/hosts on first site
    *  Fallback language
    *  Revert partial css files
    *  mod_admin: fix position of drag icon in connection item
    *  mod_admin: Update Dutch translations
    *  mod_menu: A couple of Dutch translations
    *  mod_menu: Improve text (and remove html markup)
    *  mod_menu: Fix fuzzy Dutch translations
    *  mod_admin_predicate: Fix string escaping in translation text
    *  mod_admin: Fix extraneous spaces; fix fuzzy Dutch
    *  mod_l10n: typo
    *  mod_admin: tweak layout top header
    *  mod_admin: tweak widget colors on edit page
    *  mod_admin: tweak colors dashboard
    *  mod_admin: remove period from "No items."
    *  Fix translations by unmatched spaces in text
    *  mod_admin: Edit category dialog: add button to pages
    *  mod_admin: improve layoud edit form in dialog
    *  mod_admin: Fix markup in translation text
    *  mod_admin: tweak colors dashboard: couple of bug fixes
    *  mod_admin: add test query button
    *  mod_admin: tweak dashboard and dialog navs
    *  mod_translation: Give feedback when no languages can be copoed
    *  mod_admin: update translations
    *  mod_admin: update translations with msguniq
    *  docs: update admin layout cookbook
    *  mod_admin: couple of translation fixes
    *  mod_admin_identity: update translation files; add NL translations
    *  mod_admin: add button container to widget header if it is not present in the template
    *  Small fixes
    *  docs: update cookbook for dynamic select boxes
    *  mod_admin: hide minimize button in dialog
    *  mod_admin: tweak menu item style
    *  mod_admin: make connections list reusable
    *  mod_admin: make connections list reusable: fix button label, shorten add link and update translations
    *  mod_admin: fix dialog header translation
    *  docs: update info about translations

Maas-Maarten Zeeman (7):

    *  mod_base: Only use the websocket when it is in OPEN state.
    *  mod_backup: Catch errors when restoring a resource. Better user feedback in case of problems.
    *  authentication: Set user preferences like language and tz during a logon.
    *  core: Show an error when there is a problem rendering error.tpl
    *  core: Set reqdata in context when rendering error.tpl. Fixes #1013
    *  core: Make sure the error template is rendered with the expected variables and a good context.
    *  core: Removed error logging code on ws error handler.

Marc Worrell (19):

    *  mod_acl_user_groups: show default user group if user is not member of any group.
    *  mod_acl_user_groups: when displaying user's groups, check for displayed user being an user.
    *  mod_admin: in dialog_edit_basics, only show the 'full edit' button if the user can access either mod_admin or mod_admin_frontend
    *  core: refactored pivot queue polling, now will poll faster if anything was found in the queue.
    *  core: force reload of client if page_id in z_msg is undefined.
    *  core: fix a problem where a multipart post form could loop if the end-boundary was missing
    *  mod_mqtt: fix race condition in re-subscribe of mqtt listeners after a module restart.
    *  mod_admin: also check in .css files resulting from .less compilation.
    *  mod_base: fix controller_api context handling in to_json.
    *  core: allow '1' and 'true' as timezones (both map to UTC)
    *  mod_admin: also check in .css files resulting from .less compilation.
    *  mod_base: fix a problem where a pager on page with 'id' and was not using the page_path of the resource. Fixes #1005
    *  core: set default timeout of sql queries higher (from 5sec to 30sec)
    *  mod_base: fix a problem with checking if the current path is the canonical path. Issue #1010
    *  mod_admin: add generated .css files for systems without less installed.
    *  core: change logging notifications from {log, ...} to #zlog{}. Issue #992
    *  core: fix typo in os:timestamp() call.
    *  Merge branch 'release-0.13.x' of github.com:zotonic/zotonic into release-0.13.x
    *  core: add 'www.google.com/maps/' to the sanitizer white list.

