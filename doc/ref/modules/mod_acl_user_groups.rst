
.. include:: meta-mod_acl_user_groups.rst

This module adds rule based access control.

 * All resources (pages) are assigned a content group.
 * All users are member of zero or more user groups.
 * Content groups are arranged in an hierarchy
 * User groups are arranged in an hierarchy

Rules are defined between the user groups and the content groups.
Per rule the following properties can be set:

 * User group
 * Content group
 * Category (or *all categories*)
 * Privileges: view, edit, link, delete
 * Deny flag (for a negative rule)

The collection of rules has an *edit* and *publish* version.
The *edit* version can be tested with a special url.
If all is accepted, then the *edit* version van be published.

The *edit* version can also be exported and imported.
This includes the full hierarchies of all user- and content groups.
