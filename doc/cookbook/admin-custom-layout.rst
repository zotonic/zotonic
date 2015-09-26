.. _admin-custom-layout:

Customizing the layout of the admin edit page
=============================================

Why
---

After having created a custom widget (see :ref:`admin-a-custom-widget`), we want to hide widgets that we don't need.


Assumptions
```````````

Readers are expected to have experience with Zotonic templates. For reference, look at the Zotonic admin template directory ``modules/mod_admin/templates/``.



Page structure
--------------

To hide default widgets from the page, we need to change the template that loads these widgets. 

The edit page widgets are grouped into 2 main areas, main and sidebar. These groups are created by the files ``_admin_edit_main_parts.tpl`` and ``_admin_edit_sidebar_parts.tpl``.


Content parts
-------------

We won't change the default Zotonic edit template; instead we will override it by adding the category to the filename.

Create a file ``_admin_edit_main_parts.webshop.tpl`` and give it the contents::

    {% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
    {% all catinclude "_admin_edit_content.tpl" id is_editable=is_editable languages=languages %}
    {% include "_admin_edit_content_advanced.tpl" %}

This template will now load the Basic widget (Title, Summary, Short title), all content widgets (for now only ``_admin_edit_content.webshop.tpl``), and the default Advanced widget.


Sidebar
-------

For the sidebar area you can follow the same procedure. Of course you need to keep the submit buttons, so always include ``_admin_edit_content_publish.tpl``.


Category and instance
---------------------

To show a block on a category edit page, but not on an instance of that category, check for ``r.is_a.meta``. To show the block "Features" on the category page we write::

  {% if r.is_a.meta %}
  {% include "_admin_edit_meta_features.tpl" %}
  {% endif %}


Word of caution
---------------

Admin templates will evolve over time. Custom changes you make now may not work after an update. Use these guidelines at your own risk.

