{% if m.acl.use.mod_admin %}
    {% lazy template="_clamav_admin_status_alert.tpl" %}
{% endif %}
