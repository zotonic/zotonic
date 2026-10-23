{% extends "page.tpl" %}

{% block title %}{_ Test Access Control Rules _}{% endblock %}

{% block content %}
	<h1>{_ Test Access Control Rules _}</h1>

	{% with m.acl_rule.acl_user_groups_state == `edit` as is_test %}
	{% with q.code and m.acl_rule.is_valid_code[q.code] as is_valid_code %}
		<p class="alert alert-info" role="status">
			{% if is_test %}
				{_ Test access control rules are active. Select Published rules to stop testing. _}
			{% else %}
				{_ Published access control rules are active. _}
			{% endif %}
		</p>

		<div class="btn-group" role="group" aria-label="{_ Access control rules _}">
			<button type="button" id="{{ #acl_publish }}" class="btn btn-default{% if not is_test %} active{% endif %}"
				aria-pressed="{% if is_test %}false{% else %}true{% endif %}" {% if not is_test %}disabled{% endif %}>
				{_ Published rules _}
			</button>
			<button type="button" id="{{ #acl_edit }}" class="btn btn-warning{% if is_test %} active{% endif %}"
				aria-pressed="{% if is_test %}true{% else %}false{% endif %}" {% if is_test or not is_valid_code %}disabled{% endif %}>
				{_ Test rules _}
			</button>
		</div>

		{% if is_test %}
			{% wire id=#acl_publish
					action={mask target=" body"}
					postback={switch_rule_state code=q.code state=`publish`}
					delegate=`admin_acl_rules_rsc`
			%}
		{% elseif is_valid_code %}
			{% wire id=#acl_edit
					action={mask target=" body"}
					postback={switch_rule_state code=q.code state=`edit`}
					delegate=`admin_acl_rules_rsc`
			%}
		{% endif %}

		{% if not is_valid_code %}
			<p class="alert alert-warning">
				{_ To start testing, use a valid test link provided by an editor or administrator. _}
			</p>
		{% endif %}
		<p>{_ This test link is valid for 24 hours after it is generated. _}</p>
	{% endwith %}
	{% endwith %}
{% endblock %}
