
.. include:: meta-mod_acl_simple_roles.rst

Locking down access to pages and features based on users and groups,
this module enables a simple role-based access model for the Zotonic
admin.

Users are part of one or more ACL roles. Each ACL roles dictates which
resource categories its members can edit.
   
Enabling role-based access control (ACLs)
-----------------------------------------

As a site gets larger and the team working on it grows it becomes very
important to have some segregation of duties around who gets to
control or change what on the site.  This guide provides step-by-step
instructions for enabling ACL permissions in Zotonic to facilitate
these kinds of controls.
             

Assumptions
...........

Readers are expected to be familiar with the Zotonic CMS admin
interface and be comfortable working with the Modules section to
enable and disable Zotonic Modules.

How
...

- Activate module `ACL Simple Roles` and disabled `ACL Admin Only`
- Create a new user, e.g. `greg`.
- Created new role named `Posting Members` check categories `Text` and `Media`.
- Also chech the module `Admin`.
- Save
- Added in Page Connections in ACL Role Member user greg (search by full user name)
- Save
- Log out
- Log in as greg. You should see only Pages and Media buttons.
