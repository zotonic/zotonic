
.. include:: meta-acl.rst

The m_acl model gives access the id of the currently logged in user,
and provides a mechanism to do basic access control checks.

The following m_acl model properties are available in templates:

+--------------------+--------------------------------------+
|Property            |Description                           |
+====================+======================================+
|user                |Returns the current user id. If not   |
|                    |logged in, this returns ``undefined``.|
+--------------------+--------------------------------------+
|is_admin            |Check if the current user is alllowed |
|                    |to access the admin. Internally, this |
|                    |checks the ``use, mod_admin_config``  |
|                    |ACL.                                  |
+--------------------+--------------------------------------+
|use, admin, view,   |These properties are shortcuts to     |
|delete, update,     |check if the current user is allowed  |
|insert, link        |to do some action.                    |
+--------------------+--------------------------------------+
|is_allowed          |Perform custom ACL checks which are   |
|                    |different from the ones mentioned.    |
+--------------------+--------------------------------------+
|authenticated       |Used before the other ACL checks to   |
|                    |check if a *typical* user is allowed  |
|                    |to perform some actions. Example:     |
|                    |``m.acl.authenticated.insert.article``|
|                    |If an user is logged on the that      |
|                    |user’s permissions are used.          |
+--------------------+--------------------------------------+


.. highlight:: django

This example prints a greeting to the currently logged in user, if
logged in::

  {% if m.acl.user %}
      Hello, {{ m.rsc[m.acl.user].title }}!
  {% else %}
      Not logged in yet
  {% endif %}

This example checks if the user can access the admin pages::

  {% if m.acl.is_admin %} You are an admin {% endif %}


This example performs a custom check::

  {% if m.acl.is_allowed.use.mod_admin_config %}
      User has rights to edit the admin config
  {% endif %}

And to check if a resource is editable::

   {% if m.acl.is_allowed.update[id] %}
      User can edit the resource with id {{ id }}
   {% endif %}

A short hand for the above is (assuming `id` is an integer)::

   {% if id.is_editable %}
      User can edit the resource with id {{ id }}
   {% endif %}
