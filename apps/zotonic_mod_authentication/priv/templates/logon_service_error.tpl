{% extends "base_simple.tpl" %}

{# Template shown in popup window after redirecting to an external identity service #}

{% block title %}
	{% with error|default:q.error as error %}
		{% if error == 'confirm' or error == 'need_passcode' or error == 'passcode' %}
			{_ Confirm _}
		{% else %}
			{_ Error _}
		{% endif %}
	{% endwith %}
{% endblock %}

{% block content %}
	{% include "_logon_service_error.tpl" %}
{% endblock %}
