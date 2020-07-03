{% extends "base_simple.tpl" %}

{# Template shown in popup window when redirecting to an external identity service #}

{% block title %}{_ Authorizing... _}{% endblock %}

{% block content %}
	{% lazy "_logon_service."++q.service++".tpl" service=q.service is_connect=q.is_connect %}
{% endblock %}
