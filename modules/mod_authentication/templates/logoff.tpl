{% extends "base.tpl" %}

{% block title %}{_ Log Off _} ...{% endblock %}

{% block html_head_extra %}
	<meta http-equiv="refresh" content="6;url=/" />
{% endblock %}

{% block content_area %}
	<h1>{_ One moment please, logging off… _}</h1>
	
	<p>{_ You will be redirected to the home page. _}</p>
	
	{% all include "_logoff_extra.tpl" %}
{% endblock %}
