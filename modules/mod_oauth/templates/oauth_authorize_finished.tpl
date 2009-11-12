{% extends "base.tpl" %}

{% block title %} Authorize this request {% endblock %}

{% block content %}
	<div id="content" class="zp-100">
        <h1>Authorized!</h1>

        <p>You have granted the application <em>{{ token.application_title|default:"Untitled" }}</em> access your account.</p>

        <p>You can now safely close this window.</p>
        
	</div>
{% endblock %}

