{% if m.acl.is_admin or m.acl.use.mod_site_update %}
    {% live topic="bridge/origin/$SYS/status/code-update" template="_site_deploy_status_value.tpl" %}
{% endif %}
