{% if m.acl.is_allowed.use.mod_admin_identity or id == m.acl.user %}
{% live template="_admin_identity_email.tpl" topic=["~site", "rsc", id, "identity"] id=id %}
{% endif %}
