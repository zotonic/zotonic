.. _rel-0.76.0:

Release 0.76.0
==============

Welcome to Zotonic 0.76.0, released on April 4, 2023.

This is a maintenance release.

Main changes are:

 * Fix for access checks on collaboration groups in the presence of deny rules
 * Fix for a problem where ACL checks could fail after the same process updated the ACL properties
 * Added the acl_collab_groups_modify notification from the master branch

Commits since 0.75.0
--------------------

Colin de Roos (3):

 * Flush ACL cache on resource update (#3361)
 * Allow inserting acl rules with action strings (#3360)
 * Fix deny rules for specific collaboration groups (0.x) (#3359)

Marc Worrell (6):

 * mod_logging: be less vocal about the database pool usage (#3323)
 * core: on rsc update error for expected version, return error (#3322)
 * core: on insert of duplicate task, handle error more gracefully (0.x) (#3338)
 * core: on rsc update expect error, flush resource cache (#3343)
 * mod_acl_user_groups: add acl_collab_groups_modify notification (0.x) (#3357)
 * mod_admin: fix a problem where page_path was multiple time url encoded (#3363)
