<div id="sort"> {# also sidebar #}

{# Publish page #}
{% include "_admin_edit_content_publish.tpl" headline="simple" %}

{% if r.is_a.meta %}
{% include "_admin_edit_meta_features.tpl" %}
{% endif %}

{# Access control #}
{% include "_admin_edit_content_acl.tpl" %}

{% if not r.is_a.meta %}
        {# Publication period #}
{% include "_admin_edit_content_pub_period.tpl" %}

{# Date range #}
{% include "_admin_edit_content_date_range.tpl" %}
{% endif %} {# not r.is_a.meta #}

{% all catinclude "_admin_edit_sidebar.tpl" id languages=languages %}

{# Page connections #}
{% include "_admin_edit_content_page_connections.tpl" %}
</div>