{% extends "base_simple.tpl" %}

{# Template shown in popup window when redirecting to an external identity service #}

{% block title %}{_ Authenticated _}{% endblock %}

{% block content %}
	<div class="container">
		<p class="alert">{_ Authentication has been done with _} {{ service }}</p>
	</div>
	{% javascript %}
		window.close();
	{% endjavascript %}
{% endblock %}
