Customizing the layout of an admin edit page
============================================

Why
---

The default admin edit pages are a "one size fits all". There are times you want to customize which elements you want to show on a specific edit page.

This recipe explains how to create a custom widget block on the page.

Assumptions
-----------

Readers are expected to have experience with Zotonic templates. For reference, look at the Zotonic directory ``modules/mod_admin/templates/``.

How
---

For a imaginary Shop edit page, we want to hide some of the redundant widgets (address), while adding a new one that only contains the geo location and url.

The edit page widgets are grouped into 2 main areas, main and sidebar. These groups are reflected in ``_admin_edit_main_parts.tpl`` and ``_admin_edit_sidebar_parts.tpl``.

Widgets are included with a "category include". We can create a new "Shop category" version by including the category name (lowercase) in the filename.

Structure
`````````

We create our "main part" template ``_admin_edit_main_parts.shop.tpl`` and give it the contents::

    {% all catinclude "_admin_edit_basics.tpl" id is_editable=is_editable languages=languages %}
    {% include "_admin_edit_shop.tpl" %}
    {% include "_admin_edit_content_advanced.tpl" %}

This template will include basics widget (Title, Summary, Short title), then include a new template ``_admin_edit_shop.tpl``, and finish with the default Advanced widget.


Widget
``````
Our widget will extend the base widget. To give it translations attributes, we extend ``admin_edit_widget_i18n.tpl``, but in our case the translation tabs will be in the way so we will work with ``admin_edit_widget_std.tpl``.

Extending will give us a couple of default properties like ``id``.

Create a file ``_admin_edit_shop.tpl``::

    {% extends "admin_edit_widget_std.tpl" %}

    {% block widget_title %}
        {_ Shop Properties _}
    {% endblock %}

    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}edit-shop-properties{% endblock %}

    {% block widget_content %}
        {% with m.rsc[id] as r %}
        {% with m.rsc[id].is_editable as is_editable %}
        <fieldset class="form-horizontal">
            <div class="form-group row">
                <label class="control-label col-md-3">{_ URL _}</label>
                <div class="col-md-9">
                    <input type="text"
                        name="url" 
                        class="form-control"
                        value="{{ r.url }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
        </fieldset>
        {% endwith %}
        {% endwith %}
    {% endblock %}

Of course more fields can be added within the ``widget_content`` block.


Sidebar
```````

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

