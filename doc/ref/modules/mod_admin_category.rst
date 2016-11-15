
.. include:: meta-mod_admin_category.rst

Add support for editing :ref:`guide-datamodel-categories` in the
admin, by presenting an editable category tree at
``http://yoursite.com/admin/category``.

.. note:: This module requires the presence of :ref:`mod_menu` for the
          required JavaScript files which make up the menu editor.

ACL permissions
---------------

The following :ref:`ACL permissions <guide-authorization>` are required:

- to view the page, :ref:`use permission <module-acl>` on the
  ‘mod_admin_category’ module
- to view the list of categories, :ref:`view permissions <content-acl>` on
  category ‘category’
- to edit and re-order the categories, :ref:`edit permissions <content-acl>`
  on category ‘category’.

.. todo:: Add more documentation
