{% if m.acl.use.mod_admin %}
    {% if show_status %}
        {% include "_admin_status_check.tpl" check_title=_"Virus scanner" check_template="_clamav_admin_status_alert.tpl" %}
    {% else %}
        {% lazy template="_clamav_admin_status_alert.tpl" %}
    {% endif %}
{% endif %}
