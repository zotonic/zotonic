{% include "_admin_acl_rule_header.tpl" %}

{% for rule in m.acl_rule[kind].edit %}
    {% include "_admin_acl_rule_row.tpl" rule=rule %}
{% empty %}
    {_ No ACL rules _}
{% endfor %}
