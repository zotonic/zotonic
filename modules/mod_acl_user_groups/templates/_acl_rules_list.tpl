{% include "_admin_acl_rule_header.tpl" %}

{% if is_editable %}
	{% for rule in m.acl_rule[kind].edit[{all group=group}] %}
	   {% include "_admin_acl_rule_row.tpl" rule=rule %}
	{% empty %}
	    <span class="muted">{_ No ACL rules _}</span>
	{% endfor %}

	{% javascript %}
	    var elements = $("select[name=acl_user_group_id]");
	{% endjavascript %}
{% else %}
	{% for rule in m.acl_rule[kind].publish[{all group=group}] %}
	   {% include "_admin_acl_rule_row.tpl" rule=rule %}
	{% empty %}
	    <span class="muted">{_ No ACL rules _}</span>
	{% endfor %}
{% endif %}
