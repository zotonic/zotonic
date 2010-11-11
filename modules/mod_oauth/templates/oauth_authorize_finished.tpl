{% extends "base.tpl" %}

{% block title %}{_ Authorization finished _}{% endblock %}

{% block content %}
	<div id="content" class="zp-100">
        <h1>{_ Authorized! _}</h1>

        <p>{_ You have granted the application _} <em>{{ token.application_title|default:"Untitled" }}</em> {_ access your account. _}</p>

        <p>{_ You can now safely close this window. _}</p>

	</div>
{% endblock %}

