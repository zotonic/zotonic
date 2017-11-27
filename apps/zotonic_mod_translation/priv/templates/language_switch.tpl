{% extends "base.tpl" %}

{% block title %}{_ Select language _}{% endblock %}

{% block content %}

<p>{_ Select your preferred language. _}</p>

<ul class="language-switch nav nav-list">
    {% for code,lang in m.translation.language_list_enabled %}
    	<li{% if z_language == code %} class="disabled"{% endif %}>
    	    <a href="{% if z_language != code %}{% url language_select code=code p=q.p %}{% endif %}">{{ lang.name }}</a>
    	</li>
    {% endfor %}
</ul>

{% endblock %}
