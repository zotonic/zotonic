.. _rel-0.45.0:

Release 0.45.0
==============

Welcome to Zotonic 0.45.0, released on 29 January, 2019.

Main changes are:

 * ACL fixes, especially for upload restrictions

Commits since 0.44.0
--------------------

Maas-Maarten Zeeman (1):

 * Use new_request to initialize the context for a new request (#1976)

Marc Worrell (9):

 * mod_seo: Fix a problem with Google Analytics settings.
 * mod_authentication: check if user has an username on expired status.
 * mod_base: better activity tracking
 * mod_editor_tinymce: fix problem with tinymce init in dialog on firefox
 * mod_acl_user_groups: fix a problem where the ACL settings for file-types were not visible.
 * mod_acl_user_groups: fix a problem where the max-upload file size was not properly checked.
 * mod_acl_user_groups: show default ACL mime setting if not set in group.
 * mod_acl_user_groups: fix column width for mime-rule view.
 * Set version to 0.45.0 (#1980)

loetie (1):

 * Badmatch in expired logon on get_username value (#1977)

