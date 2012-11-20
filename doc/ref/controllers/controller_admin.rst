
.. include:: meta-admin.rst

The admin controller is the main controller behind which admin pages
are served. Its main purpose is that it does an authentication check
(Is current user allowed to ``use`` the module ``mod_admin``).

The `template` parameter decides which admin template gets served, and
defaults to `admin.tpl`.

.. todo:: Extend documentation
