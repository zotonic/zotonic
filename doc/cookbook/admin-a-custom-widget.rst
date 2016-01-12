.. _admin-a-custom-widget:

Creating a custom widget on the edit page
=========================================

Why
---

For an imaginary webshop edit page, we want to add 2 more data fields: the affiliate URL and a note about free shipping.


Assumptions
```````````

Readers are expected to have experience with Zotonic templates. For reference, look at the Zotonic admin template directory ``modules/mod_admin/templates/``.


Custom widget
-------------

Content widgets are created by creating a file ``_admin_edit_content.my_cat.tpl``, where ``my_cat`` is the category name - ``webshop`` in our case.

Create a file ``_admin_edit_content.webshop.tpl``::

    {% extends "admin_edit_widget_std.tpl" %}

Extending will also give us a couple of default properties like ``id`` and ``is_editable``.

We can also override ``admin_edit_widget_i18n.tpl`` if fields need to get translated.

Add these block definitions::

    {% block widget_title %}
        {_ Webshop Properties _}
    {% endblock %}

    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}edit-webshop-properties{% endblock %}

    {% block widget_content %}
        <fieldset class="form-horizontal">
            <div class="form-group row">
                <label class="control-label col-md-3">{_ Affiliate URL _}</label>
                <div class="col-md-9">
                    <input type="text"
                        name="affiliate_url" 
                        class="form-control"
                        value="{{ id.affiliate_url }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
            <div class="form-group row">
                <label class="control-label col-md-3">{_ Free shipping note _}</label>
                <div class="col-md-9">
                    <input type="text"
                        name="free_shipping_note" 
                        class="form-control"
                        value="{{ id.free_shipping_note }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
        </fieldset>
    {% endblock %}


When loading a webshop edit page, the widget should now appear below the Basics block.

To customize the edit page further, see :ref:`admin-custom-layout`.


Word of caution
---------------

Admin templates will evolve over time. Custom changes you make now may not work after an update. Use these guidelines at your own risk.

