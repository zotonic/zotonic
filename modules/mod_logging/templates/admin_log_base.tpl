{% extends "admin_base.tpl" %}

{% block title %}{_ Log _}{% endblock %}

{% block content %}

<div class="edit-header">
	
    <h2>{% block title_log %}{_ Log messages _}{% endblock %}</h2>
    <br />
    <ul class="nav nav-tabs">
	<li class="{% block active1 %}{% endblock %}"><a href="{% url admin_log %}">{_ Message log _}</a></li>
	<li class="{% block active2 %}{% endblock %}"><a href="{% url admin_log_email %}">{_ Email log _}</a></li>
    </ul>
	    
    {% block content_log %}
    {% endblock %}

</div>

{% endblock %}
