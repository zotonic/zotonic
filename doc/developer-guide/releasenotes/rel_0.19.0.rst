.. _rel-0.19.0:

Release 0.19.0
==============

Welcome to Zotonic 0.19.0, released on 1 August, 2016.

Main changes are:

* Removed module mod_acl_simple_roles from core (:issue:`1328`). Please use
  :ref:`mod_acl_user_groups` instead. If you need mod_acl_simple_roles, the module
  is available in a `separate repository <https://github.com/zotonic/mod_acl_simple_roles>`_.
* Added `site tests <http://zotonic.com/docs/latest/developer-guide/testing.html>`_
  (:issue:`1331`).
* Added View dropdown menu in admin (:issue:`1345`).
* Added possibility to return `JSON error objects <http://zotonic.com/docs/latest/developer-guide/services.html#working-with-error-objects>`_
  from API service controllers.
* Added ``m.req.is_bot`` (:issue:`1340`).

Commits since 0.18.0
--------------------

There were 22 commits since release 0.18.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arjan Scherpenisse (9):

   * Implement running site-specific tests (#1332)
   * core: Add z_sitetest:{watch,unwatch} to speed up test driven development
   * API: Allow process_{post,get} to throw() error for shortcircuiting
   * core: Only run sitetests when compilation succeeds
   * mod_acl_user_groups: Documentation fixes; change 'edit' -> 'update'
   * API: Fix logic bug in try/catch clause handling for process_{get,post}
   * doc: Add note that testname cannot containing underscores
   * API: Log crashes, serve HTTP 500 on uncaught exception
   * filewatcher: reload module first when running all sitetests for a site

Arthur Clemens (3):

   * docs: Describe error handling from API service
   * Fix include_lib
   * mod_base: controller_api: add option to pass JSON error object

David de Boer (4):

   * core: Move mod_acl_simple_roles out of core into separate repo (#1328)
   * doc: Add .pot generation function call
   * admin: Add view dropdown menu (#1345)
   * doc: Fix build

Marc Worrell (6):

   * core: add forward-compatible z_utils:name_for_site/2. Issue #1333
   * mod_admin: add possibility to disconnect connections via the connection-dialog Fixes #1339
   * core: add 'is_bot' property for m.req Fixes #1340
   * core: use the ua_classifier supplied is_crawler flag. Fallback to the hardcoded list for unknown user agents (example: curl). Fixes #1340
   * core: in z_user_agent allow WebSocket and websocket.
   * z_user_agent: make the upgrade comparison case insensitive
