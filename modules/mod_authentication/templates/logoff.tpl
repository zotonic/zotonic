{% extends "base.tpl" %}

{% block title %}{_ Sign out _} ...{% endblock %}

{% block html_head_extra %}
	<meta http-equiv="refresh" content="4;url={{ q.p|escape|default:"/"}}" />
{% endblock %}

{% block content_area %}
	<h1>{_ One moment please, signing out... _}</h1>

	<p>{_ You will be redirected to the home page. _}</p>

	{% all include "_logoff_extra.tpl" %}
{% endblock %}

