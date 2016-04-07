{% with id.email_raw as email %}
	{% if email and not m.email_status.is_valid[email] %}
		{% if email_status_tag %}
			<{{email_status_tag}} id="{{ #status }}" class="text-error email-status-flag">
		{% else %}
			<a href="#" id="{{ #status }}" class="text-error email-status-flag" title="{_ There are problems with this email address. _}">
		{% endif %}
				<span class="glyphicon glyphicon-envelope"></span>
				<span class="text">{_ There are email problems. _}</span>
		{% if email_status_tag %}
			</{{email_status_tag}}>
		{% else %}
			</a>
		{% endif %}
		{% wire id=#status action={dialog_open title=email|escape template="_dialog_email_status.tpl" id=id} %}
	{% endif %}
{% endwith %}
