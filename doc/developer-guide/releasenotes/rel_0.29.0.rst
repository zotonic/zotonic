.. _rel-0.29.0:

Release 0.29.0
==============

Welcome to Zotonic 0.29.0, released on 5 June, 2017.

Main changes are:

* Added an Exometer metric for the number of depcache evictions (:issue:`1682`).
* Added Erlang maps support in ErlyDTL (:issue:`1684`) and ``moresults`` action
  (:issue:`1689`).
* Added ISO 8601 date support to ``date`` filter (:issue:`1702`).
* Added a :ref:`cookbook-exometer` cookbook (:issue:`1679`) and a
  :ref:`guide-deployment-server-configuration` chapter to the documentation
  (:issue:`1680`).
* Fixed ``erlang.config`` not read during sitetest (:issue:`1672`).
* Fixed race condition in notifications during database transaction
  (:issue:`1693`).
* Fixed ErlyDTL compilation generating too many atoms (:issue:`1673`).
* Fixed wav mimetype for Internet Explorer (:issue:`1661`).

Commits since 0.28.0
--------------------

There were 24 commits since release 0.28.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (12):
    * core: Add ErlyDTL maps support (#1684)
    * doc: Bump year
    * core: Ignore priv/filezcache
    * tests: Read erlang.config in sitetest command (#1672)
    * mod_base: Support maps result in moreresults filter (#1689)
    * doc: Fix build (#1694)
    * doc: Fix RTD theme's CSS not loaded
    * core: Record depcache evictions in exometer (#1682)
    * doc: Add server configuration chapter
    * mod_base: Support ISO 8601 dates in filter_date (#1702)
    * core: Delay notifications during database transactions (#1693)
    * mod_editor_tinymce: Don't override language if already set (#1697)

Maas-Maarten Zeeman (1):
    * doc: Added information on increasing nf_conntrack_buckets

Marc Worrell (9):
    * core: fix a problem where compilation of templates generates random atoms. Fixes #1673 (#1674)
    * mod_acl_user_groups: fix a problem with insert checks without passed props.
    * core: fix a problem with .wav mime type (#1678)
    * core: don't crash in rsc_gone on race conditions.
    * mod_oauth: use the user-id from the app-token, not the consumer.
    * Upgrade z_stdlib
    * Lock new z_stdlib
    * Upgrade z_stdlib
    * mod_email_status: better transient error handling.

Marco Wessel (1):
    * doc: Add Exometer cookbook (#1679)

yorivanloo (1):
    * translation: 'bedank' to 'bedankt' (#1681)
