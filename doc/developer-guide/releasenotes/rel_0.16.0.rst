.. _rel-0.16.0:

Release 0.16.0
==============

Welcome to Zotonic 0.16.0, released on 2 May, 2016.

Main changes are:

* Fixed :issue:`1099`: added collaboration groups.
* :issue:`1227`: Added `ip_whitelist configuration option <http://zotonic.com/docs/0.x/ref/configuration/site-configuration.html>`_
  to block unwanted access when admin password is ‘admin’.
* Serve status site with 503.
* Fixed :issue:`1229`: users cannot update their person resource.
* Fixed :issue:`1236` by removing is_authoritative access check.
* Fixed :issue:`1245` by improving the 403 page.
* Fixed :issue:`1147` by passing ``qargs`` to search.
* Fixed :issue:`1230`: Firefox file upload error.
* Fixed :issue:`1248`: Dutch month abbreviation.
* Fixed :issue:`1250`: view button in case of no update permissions.

Commits since 0.15.0
--------------------

There were 39 commits since release 0.15.0.

Big thanks to all people contributing to Zotonic!

Git shortlog
............

Arjan Scherpenisse (4):
      * zotonic_status: Serve the root page with HTTP 503
      * mod_acl_user_groups: Fix permission issue
      * mod_acl_user_groups: Fix typo in edit check on the collab group itself
      * scripts: Fix prepare-release.sh

David de Boer (5):
      * mod_acl_user_groups: Automatically publish managed ACL rules
      * doc: Add Sphinx extlinks extension
      * mod_l10n: Fix #1248 Dutch month abbreviation
      * doc: Update for new lossless default (6e5692d0cab0eb5a125a05a3ab02933a0e6829e4)
      * Prepare release 0.16.0

Marc Worrell (29):
      * Add a whitelist of IP addresses that can use the default 'admin' password to login into sites.
      * Add doc for ip_whitelist config. Fix ip6 netmask
      * mod_search: add search argument 'qargs'
      * mod_admin: add 'object_id' as optional argument to the connect dialog. Remove template from the 'update' notification, as it is '_rsc_item.tpl' anyway.
      * modules: correction for some icons and small textual changes.
      * mod_admin_predicate/category: fix a problem with moving categories and predicates.
      * Add 169.254.0.0/16 and fe80::/10 to the default ip_whitelist
      * core: add z_module_manager:activate_await/2, z_notifier:await/2 and /3.
      * core: rename z_module_manager:activate_wait to activate_await
      * mod_base: fix a problem with Firefox when uploading files via the postback controller. Fixes #1230
      * mod_base: fix a problem with firefox and sortable. Fixes #1239
      * mod_acl_user_groups: added support for collaboration groups. Issue #1099
      * Fix a problem where an user can't update their own 'person' resource. Issue #1229
      * mod_acl_user_groups: fix a problem where 'sudo' was not handled properly during rsc updates. Issue #1229
      * mod_acl_user_groups: remove default 'view' rights on collaboration group.
      * core: do not require additional permissions to change the is_authoritative flag. Fixes #1236
      * docs: remove mention of streamhost, as it is removed from the code. Fixes #1228
      * mod_survey: changes for editing surveys and readable display of survey answers.
      * mod_export: fixes for export.
      * mod_export: fix header value lookup for {trans, _} tuples.
      * mod_base: in filter temporary_rsc, don't crash on insert access error but return 'undefined'
      * mod_admin: on edit page, make 'view' button into an anchor if no edit rights. Fixes #1250
      * mod_acl_user_groups: do not give view rights on unpublished rsc, unless user has edit permission.
      * mod_authentication/mod_base: better 403 page and translations. Fixes #1245
      * mod_acl_user_groups: 'edit' should be 'update', use m_rsc version of is_published_date
      * core: ensure in m_rsc that 'language' is a list of known languages.
      * mod_signup: use foldr for signup_form_fields, let higher prio modules win.
      * mod_acl_user_groups: tune access permissions for collaboration groups.     All collab group members can view the collab group.     If someone can update/link/delete a collab group, then that user can do the same on the collab group content.     Rename the config collab_group_edit to collab_group_update.
      * mod_acl_user_groups: members of a collaboration group can view each other.

Osei Poku (1):
      * doc: Fix minor errors in documentation
