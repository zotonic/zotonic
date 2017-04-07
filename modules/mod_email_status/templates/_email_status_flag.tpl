{% with id.email_raw as email %}
	{% if email and not m.email_status.is_valid[email] %}
	{% with m.email_status[email] as status %}
		{% if email_status_tag %}
			<{{email_status_tag}} id="{{ #status }}" class="{% if status.error_is_final %}text-error{% else %}text-warning{% endif %} email-status-flag">
		{% else %}
			<a href="#" id="{{ #status }}" class="{% if status.error_is_final %}text-error{% else %}text-warning{% endif %} email-status-flag" title="{_ There are problems with this email address. _}">
		{% endif %}
				<span class="glyphicon glyphicon-envelope"></span>
				<span class="text">{_ There are email problems. _}</span>
		{% if email_status_tag %}
			</{{email_status_tag}}>
		{% else %}
			</a>
		{% endif %}
		{% wire id=#status action={dialog_open title=email|escape template="_dialog_email_status.tpl" id=id} %}
	{% endwith %}
	{% endif %}
{% endwith %}
