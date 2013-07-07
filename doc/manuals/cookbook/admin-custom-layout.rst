Customizing the layout of an admin edit page
============================================

Why
---

The default admin edit pages are a "one size fits all". There are times you want to customize which elements you want to show on a specific edit page.

Assumptions
-----------

Readers are expected to have experience with Zotonic templates.

How
---

The edit page consists of 2 main areas, main and sidebar. The area contents are specified in sub-template files (both in ``modules/mod_admin/templates/``):

- main area: ``_admin_edit_main_parts.tpl``
- sidebar area: ``_admin_edit_sidebar_parts.tpl``

Let's say you have created a category 'Shop', and the only data you want to maintain on shops are the name and url.

In your site's template directory you create the file ``_admin_edit_main_parts.shop.tpl``. In it you write only those elements that you need. For instance, to show the default Title and Advanced blocks you would write::

  {% block admin_edit_form_top %}{% endblock %}
  
  {% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
  
  {% include "_admin_edit_content_advanced.tpl" %}

To make the Title section contain your shop's name and url, you can redefine ``_admin_edit_basics_form.tpl``. Copy the file ``_admin_edit_basics_form.tpl`` to ``_admin_edit_basics_form.shop.tpl`` in your site's template directory. Change the fields to your needs.

For the sidebar area you can follow the same procedure. Of course you need to keep the submit buttons, so include ``_admin_edit_content_publish.tpl``.


Category and instance
---------------------

To show a block on a category edit page, but not on an instance of that category, check for ``r.is_a.meta``. To show the block "Features" on the category page we write::

  {% if r.is_a.meta %}
  {% include "_admin_edit_meta_features.tpl" %}
  {% endif %}


Word of caution
---------------

Admin templates will evolve over time. Custom changes you make now may not work after an update. Use these guidelines at your own risk.

