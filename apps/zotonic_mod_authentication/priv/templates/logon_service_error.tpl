{% extends "base_simple.tpl" %}

{# Template shown in popup window when redirecting to an external identity service #}

{% block title %}{% if error == `signup_confirm` %}{_ Confirm _}{% else %}{_ Error _}{% endif %}{% endblock %}

{% block content %}
	{% include "_logon_service_error.tpl" %}
{% endblock %}
