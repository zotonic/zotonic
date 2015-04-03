{% extends "page.tpl" %}

{% block title %}{_ Test Access Control Rules _}{% endblock %}

{% block content %}
	<h1>{_ Test Access Control Rules _}</h1>

	{% if q.code and m.acl_rule.is_valid_code[q.code] %}
		<p>{_ You can switch back to the published access control rules by: _}</p>

		<ul>
			<li>{_ Clicking on <b>Use Published Rules</b> below _}</li>
			<li>{_ Closing your browser and waiting till session is expired. _}</li>
			<li>{_ Log on _}</li>
			<li>{_ Log off _}</li>
		</ul>


		<p>{_ Click on the buttons to select the ACL version to use. _}</p>
		<p>
			<a href="#" role="button" id="{{ #acl_edit }}" class="btn btn-warning">{_ Test ACL Rules _}</a>
			<a href="#" role="button" id="{{ #acl_publish }}" class="btn btn-default">{_ Use Published Rules _}</a>

			{% wire id=#acl_edit    postback={switch_rule_state code=q.code state=`edit`}    delegate=`admin_acl_rules_rsc` %}
			{% wire id=#acl_publish postback={switch_rule_state code=q.code state=`publish`} delegate=`admin_acl_rules_rsc` %}
		</p>
	{% else %}
		{% if q.code %}
			<p class="alert alert-warning">
				{_ You have to use a special URL, as provided by the an editor or administrator. _}<br/>
				{_ The URL is only valid for a day. _}
			</p>
		{% endif %}

		{% if m.session.acl_user_groups_state == `edit` %}
			<p>
				<a href="#" role="button" id="{{ #acl_publish }}" class="btn btn-success">{_ Use Published Rules _}</a>
				{% wire id=#acl_publish postback={switch_rule_state code=q.code state=`publish`} delegate=`admin_acl_rules_rsc` %}
			</p>
		{% endif %}
	{% endif %}

	<p class="alert alert-info">
		{% if m.session.acl_user_groups_state == `edit` %}
			{_ You are using the <b>test</b> version of the access control rules. _}
		{% else %}
			{_ You are using the <b>published</b> version of the access control rules. _}
		{% endif %}
	</p>
{% endblock %}
