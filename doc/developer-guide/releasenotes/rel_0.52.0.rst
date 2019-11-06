.. _rel-0.52.0:

Release 0.52.0
==============

Welcome to Zotonic 0.52.0, released on October 6, 2019.

Main changes are:

  * Add controller option ``set_session_cookie``, set to ``false`` to not set a session cookie
  * Add a *delete* button in the tinymce media edit dialog
  * API for fetching the active language list ``/api/translation/language_list``
  * Add tinyMCE release 4.9.6
  * Fix site install using the cli addsite command; replace ``.dev`` with ``.test``
  * Fix a problem where ui logging in mod_logging could not be disabled
  * Fix a problem with resource merge where edges notifications were not sent
  * Fix a problem where path names in template debug where truncated

Commits since 0.51.2
--------------------

Dorien (1):

 * Add dispatch so pager tpl can use this (#2197)

Maas-Maarten Zeeman (1):

 * core: Implemented option to make setting the session cookie optional. (#2207)

Marc Worrell (11):

 * mod_logging: no logging if is_ui_ratelimit_check returns false. (#2203)
 * Add api to fetch language list (#2208)
 * Fix widgetmanager fallback for non JSON data
 * Show full filepath in template debug. Fix #2118 (#2218)
 * core: merge edges by inserting new ones. (#2219)
 * mod_admin: fix problem with crashes on undefined translations. Fix #2212
 * Fix problems with blog skel installation (#2201)
 * mod_editor_tinymce: add zmedia 'delete' button. Fixes #2105 (#2222)
 * Fix relpath display in dtl compiler. Issue #2118
 * Include z.zeditor.js widget in the admin. This enables use of 'do_zeditor' classes.
 * Tinymce 4.9.6 (#2224)
