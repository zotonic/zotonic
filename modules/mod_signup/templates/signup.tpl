{% extends "base.tpl" %}

{% block title %}{_ Sign Up _}{% endblock %}

{% block html_head_extra %}
	{% inherit %}
	{% lib 
	    "css/logon.css"
	%}
{% endblock %}

{% block content_area %}
    {% include "_signup_config.tpl" %}
{% endblock %}