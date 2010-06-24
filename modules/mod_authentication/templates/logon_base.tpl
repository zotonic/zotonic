{% extends "base.tpl" %}

{% block title %}Log on{% endblock %}
{% block sidebar %}{% endblock %}
{% block navigation %}{% endblock %}

{% block html_head_extra %}
{% lib "css/logon.css"
 	   "css/logon_logic.css"
%}
{% endblock %}

{% block content_area %}
	{% block logon_area %}
		<p>Your log on form should come here.</p>
	{% endblock %}
{% endblock %}
