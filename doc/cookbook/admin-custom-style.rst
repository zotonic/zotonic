Customizing the style of an admin page
======================================

How to make style customizations to admin pages.


Assumptions
-----------

Readers are expected to have experience with Zotonic templates.


How
---

Creating a custom style for admin pages is best done using a module:

- Create a directory for your skin, for instance ``apps/mod_skin_admin``.
- Populate the module directory with ``src/mod_skin_admin.erl``, ``src/mod_skin_admin.app.src`` file, sub directories ``priv/lib/css/`` and ``priv/templates/`` (see also: :ref:`guide-module-structure`).
- Put your custom CSS file (f.i. ``skin-admin.css``) in ``priv/lib/css/``.
- Create a template file ``_html_head_admin.tpl`` with the contents ``{% lib "css/skin-admin.css" %}``.
- Enable the module in admin.

.. todo:: reference to making a module using rebar3 templates (src/mod_skin_admin.erl, src/mod_skin_admin.app.src)
