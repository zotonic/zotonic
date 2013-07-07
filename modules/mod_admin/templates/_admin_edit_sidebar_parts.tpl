{% include "_admin_edit_floating_buttons.tpl" %}

<div id="sort"> {# also sidebar #}
    {% include "_admin_edit_content_publish.tpl" headline="simple" %}

    {% if r.is_a.meta %}
        {% include "_admin_edit_meta_features.tpl" %}
    {% endif %}

    {% include "_admin_edit_content_acl.tpl" %}

    {% if not r.is_a.meta %}
        {% include "_admin_edit_content_pub_period.tpl" %}
        {% include "_admin_edit_content_date_range.tpl" %}
    {% endif %}

    {% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

    {% include "_admin_edit_content_page_connections.tpl" %}
</div>
