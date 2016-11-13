
.. include:: meta-mod_admin_predicate.rst

Add support for editing :ref:`predicates <guide-datamodel-edge-predicates>` in the
admin, by presenting a list of all defined predicates on
``http://yoursite.com/admin/predicate``.

Predicates can be added, removed and edited, just like regular
:term:`resources <resource>`.

ACL permissions
---------------

The following :ref:`ACL permissions <guide-authorization>` are required:

- to view the page, :ref:`use permission <module-acl>` on the
  ‘mod_admin_predicate’ module
- to edit and delete predicates, :ref:`edit and delete permissions <content-acl>`
  on category ‘predicate’.


.. todo:: Add more documentation
