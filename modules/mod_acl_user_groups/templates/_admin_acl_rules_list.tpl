{% with is_editable|if:`edit`:`publish` as which %}
	{% if m.acl_rule[kind][which][{all group=group}] as rules %}
		<table class="table table-compact table-hover">
			<thead>
				{% include "_admin_acl_rules_list_header.tpl" %}
			</thead>
			<tbody>
				{% for rule in rules %}
				   {% include "_admin_acl_rule_row.tpl" rule=rule %}
				{% endfor %}
			</tbody>
		</table>
	{% else %}
	    <span class="text-muted">{_ No ACL rules _}</span>
	{% endif %}
{% endwith %}

{% if is_editable %}
	{% button
			class="btn btn-primary"
			text=_"Add Rule"
            action={dialog_open
                        template="_dialog_acl_rule_edit.tpl"
                        title=_"Add Rule"
                        kind=kind}
   %}
{% endif %}
