{% include "_admin_edit_floating_buttons.tpl" %}

<div id="sort"> {# also sidebar #}
    {% include "_admin_edit_content_publish.tpl" headline="simple" %}

    <h5 style="margin-top:50px">{_ Add a relation to a detail page about cookies. A link to this  page will show up in the cookie banner _}</h5>
    {% include "_admin_edit_content_page_connections.tpl" predicate_ids=[
        m.predicate.relation.id
    ] %}

    {% if r.is_a.meta %}
        {% include "_admin_edit_meta_features.tpl" %}
    {% endif %}

    {% include "_admin_edit_content_acl.tpl" %}

    {% if not r.is_a.meta %}
        {% include "_admin_edit_content_pub_period.tpl" %}
    {% endif %}

    {% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

</div>