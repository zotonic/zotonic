Customizing the style of an admin page
======================================

How to make style customizations to admin pages.


Assumptions
-----------

Readers are expected to have experience with Zotonic templates.


How
---

Creating a custom style for admin pages is best done using a module:

- In your site, create the directory ``modules`` if it doesn't exist.
- Create a directory for your skin, for instance ``mod_skin_admin``.
- Populate the module directory with the module ``.erl`` file, sub directories ``lib/css/`` and ``templates/`` (see also: :ref:`manual-module-structure`).
- Put your custom CSS file (f.i. ``skin-admin.css``) in ``lib/css/``.
- Create a template file ``_html_head_admin.tpl`` with the contents ``{% lib "css/skin-admin.css" %}``.
- Enable the module in admin.


  

