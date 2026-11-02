{% if m.acl.is_admin %}
    {% lazy template="_filestore_health.tpl" %}
{% endif %}
