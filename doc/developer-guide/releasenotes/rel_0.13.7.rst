.. _rel-0.13.7:

Release 0.13.7
==============

Welcome Zotonic 0.13.7, released on 1 February, 2016.

This is a maintenance release of Zotonic 0.13.

Main changes are:

* This release is the first in the new
  `monthly release schedule <http://zotonic.com/docs/latest/dev/releasing.html>`_.
* From now on, release tag names no longer have the ``release-`` prefix.
* Combine ``if`` and ``with`` tags in
  `one expression <http://zotonic.com/docs/latest/ref/tags/tag_if.html#if-with>`_.
* Improved performance through dispatch compiler.
* New search options: ``unfinished``, ``unfinished_or_nodate``.
* New image option ``lossless=auto`` that prevents images from being converted
  to JPG.

Commits since 0.13.6
--------------------

There were 60 commits since release 0.13.6.

Big thanks to all people contributing to Zotonic!


Git shortlog
............

Arjan Scherpenisse (2):
    * z_media_preview: Add lossless=auto option to keep images as PNG after resizing
    * z_media_preview: Remove debug statement

Arthur Clemens (4):
    * mod_admin_merge: improve error feedback
    * mod_admin_merge: show error message when trying to delete the admin page
    * mod_admin_merge: more typo fixes
    * mod_admin_merge: prepare interface for "merge only"

David de Boer (6):
    * Fix category preselection
    * Fix template not found error
    * Fix e-mail address
    * Add cookbook entry for new user's group
    * Merge pull request #1153 from DorienD/formerror
    * Prepare release 0.13.7

Dirk Geurs (1):
    * mod_base: Issue #1084: Force model queries into values more often

Dorien (5):
    * [survey] added bootstrap 3 class support, added styles for narrative question
    * Added config key for content group when user registers
    * Added documentation for mod_signup content group config
    * changed default content group to undefined
    * removed has-error class from form

Maas-Maarten Zeeman (1):
    * core: Removed usage of md5_mac and sha_mac.

Marc Worrell (39):
    * Merge pull request #1108 from DorienD/fix-bootstrap-survey
    * Merge pull request #1105 from driebit/fix-category-preselection
    * mod_video: fix preview generation.
    * docs: fix generation for modules stubs. Fixes #1091
    * mod_base: fix reference for (in)validFormClass. Issue #1107
    * mod_search: add option to define a filter term with 'or' query
    * Merge pull request #1112 from driebit/fix-template-not-found
    * mod_base: fix ubf.encoce typo. Thanks to Werner Buchert
    * core: fix crash in rsc duplicate.
    * mod_admin: show unique name in overview, slight cleanup of overview loop code.
    * core: fix a problem where a revert sql query could have twice the creator_id.
    * core: fix a problem where old file contents would be served for gzip-encodings.
    * mod_admin: fix category pre-selection for insert and connect dialog.
    * core: better fallback for unexpected file 'identify' results.
    * erlydtl: add optional 'as var' to the if/elif expressions. Fixes #1085
    * docs: add 'if-with' documentation. Issue #1085
    * erlydtl: add missing to_list/1 function from master.
    * erlydtl: merge .yrl file from master.
    * core: use dispatch compiler instead of dispatch server. Minor adaptations from the version in master. Issue #1121
    * core: better error message on depickle problems.
    * core/mod_development: fixes for dispatch-rewrites and dispatch tracing.
    * mod_development: also accept path-tokens that are strings.
    * mod_search: add 'edge.created (and friends) sort order option
    * mod_search: if ordering by edge seq then add sub-order by id, so that the order is similar to the one used by the edge model
    * m_search: log errors on invalid queries, don't crash.
    * m_search: fix empty search result for erroring queries.
    * core: fix a problem where the sign_key for zotonic_status could change between requests.
    * mod_search: make the ranking behaviour configurable.
    * mod_acl_user_groups: relax adding users to a user group. Fixes #1125
    * mod_acl_user_groups: better check for 'hasusergroup' edge insert permission.
    * mod_search: add 'unfinished' and 'unfinished_or_nodate' query terms.
    * mod_query: let 'unfinished_or_nodate' check the date_start for nodate, as often the end date is not set.
    * mod_admin_predicate: move predicates in batches of 100, take care of possible duplicate edges.
    * mod_translation: set the session language if the language is changed. Fixes #1155
    * mod_survey: replace some old bs2 size classes with bs3 classes.
    * core: fix for z_email_receive:get_host/1
    * core: fix return value for z_sites_dispatcher:get_host_for_domain/1
    * mod_admin_identity: make the error-target element optional for identity-add events.
    * docs: fix header for sending email

Paul Guyot (1):
    * Fix issue with ffprobe returning unicode text

Witeman Zheng (1):
    * add a encoder fun for the external mqtt client

