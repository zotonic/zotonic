{% if m.acl.is_admin %}
    {% if show_status %}
        {% include "_admin_status_check.tpl" check_title=_"Cloud file store" check_template="_filestore_health.tpl" %}
    {% else %}
        {% lazy template="_filestore_health.tpl" %}
    {% endif %}
{% endif %}
