{% extends "base.tpl" %}

{% block title %}{_ Select language _}{% endblock %}

{% block html_head_extra %}
    <meta name="robots" value="noindex,nofollow">
{% endblock %}

{% block content %}

<h1>{_ Select your preferred language. _}</h1>

<ul class="language-switch nav nav-list">
    {% for code,lang in m.translation.language_list_enabled %}
    	<li{% if z_language == code %} class="disabled"{% endif %}>
    	    <a href="{% url language_select code=code p=q.p %}">{{ lang.name }}</a>
    	</li>
    {% endfor %}
</ul>

{% endblock %}
