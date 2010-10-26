{% extends "logon_base.tpl" %}

{% block logon_area %}

<div id="logon_outer" class="{% if is_password_reset %}logon_password_reset{% else %}{% if is_password_reminder %}logon_reminder{% else %}logon_pw{% endif %}{% endif %}">
	{% if logon_reason %}
	<p id="logon_reason">{{ logon_reason }}</p>
	{% endif %}

	<div id="logon_box">
		
		<div id="logon_error">
			{% include "_logon_error.tpl" %}
		</div>

		<div id="logon_dialog" style="float: left">
			{% include "_logon_form_box.tpl" %}
			{% include "_logon_password_reminder.tpl" %}
			{% include "_logon_password_expired.tpl" %}
			{% include "_logon_verification.tpl" %}
			{% if is_password_reset %}
				{% include "_logon_password_reset.tpl" %}
			{% endif %}
		</div>
		
		<div style="clear: both"></div>
	</div>

	{% wire action={script script="$('#username').focus();"} %}
	
	<ul id="logon_methods">
		{% all include "_logon_extra.tpl" %}
	</ul>
	
	<p class="logon_link"><a id="logon_reminder_link" href="">{_ I forgot my username or password _}.</a></p>
	{% wire id="logon_reminder_link" action={set_class target="logon_outer" class="logon_reminder"} %}

	{% if not m.acl.user %}
		<p class="logon_link"><a id="logon_pw_link" href="">{_ Please show me the log on form _}.</a></p>
		{% wire id="logon_pw_link" action={set_class target="logon_outer" class="logon_pw"} %}
	{% endif %}

	{% all include "_logon_link.tpl" %}
</div>

{# Use a real post for all forms on this page, and not AJAX or Websockets.
   This will enforce all cookies to be set correctly. #}
{% wire action={script script="z_only_post_forms = true;"} %}

{% endblock %}
