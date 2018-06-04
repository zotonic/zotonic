.. _rel-0.40.0:

Release 0.40.0
==============

Welcome to Zotonic 0.40.0, released on 4 June, 2018.

Main changes are:

* Added translatable slug and SEO fields (:issue:`1911`).
* Added JavaScript and CSS minification (:issue:`1903`).
* Fixed surname prefixed not saved during signup (:issue:`1907`).
* Fixed duplicate username error message not shown (:issue:`1908`).

Commits since 0.39.0
--------------------

There were 11 commits since release 0.39.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (1):
    * core: Fix comet error when logging in (#1877) (#1878)

Maas-Maarten Zeeman (2):
    * jsmin (#1903)
    * mod_component: Fix needed to work with automatically minified resources

Marc Worrell (8):
    * mod_admin_category: fix a problem with moving resources to another category.
    * core: also remove ascii 0 on text pivots.
    * mod_atom: fix timezone issue in test.
    * core: drop testsandboxdb schema on site start
    * mod_signup: Fix #1755 by showing error on duplicate username (#1908)
    * core: make the slug translateable. Issue #1095 (#1911)
    * core: force edge delete/insert events on edge update of object_id.
    * core: better version of m_edge:update_nth/5
    * mod_signup: fix a problem with surname prefix on signup. (#1907)
