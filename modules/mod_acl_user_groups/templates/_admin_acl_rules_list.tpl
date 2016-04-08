{% with is_editable|if:`edit`:`publish` as which %}
	{% if m.acl_rule[kind][which][{all group=group}] as rules %}
		{% include "_admin_acl_rules_list_header.tpl" %}
		{% for rule in rules %}
		   {% include "_admin_acl_rule_row.tpl" rule=rule %}
		{% endfor %}
	{% else %}
	    <span class="text-muted">{_ No ACL rules _}</span>
	{% endif %}
{% endwith %}

{% if is_editable %}
	{% javascript %}
	    var elements = $("select[name=acl_user_group_id]");
	{% endjavascript %}
{% endif %}
