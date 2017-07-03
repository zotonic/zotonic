.. _rel-0.30.0:

Release 0.30.0
==============

Welcome to Zotonic 0.30.0, released on 3 July, 2017.

Main changes are:

* Fixed a problem with escaping characters in From/Reply-To e-mail headers.
* Fixed retrieving header info when there is no wm_reqdata.

Commits since 0.29.0
--------------------

There were 7 commits since release 0.29.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

David de Boer (1):
    * Revert "scripts: Raise maximum number of atoms (#1676)"

Maas-Maarten Zeeman (2):
    * core: Fix retrieving header info when there is no wm_reqdata
    * mod_mqtt: Provide a minified version of qlobber to save some kbs

Marc Worrell (4):
    * smtp: fix a problem with escaping certain characaters in email from/reply-to headers.
    * core: add max smtp connection per domain
    * Lock z_stdlib on 0.x branch
    * mod_acl_user_groups: use default content group if no content group defined.
