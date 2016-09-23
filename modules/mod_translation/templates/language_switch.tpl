{% extends "base.tpl" %}

{% block title %}{_ Select language _}{% endblock %}

{% block content %}

<p>{_ Select your preferred language. _}</p>

<ul class="language-switch nav nav-list">
{% with m.translation.language_list_enabled as list %}
{% for code,lang in list %}
	{% if lang.is_enabled %}
	<li>
	    <a {% if z_language == code %}class="current"{% endif %} href="{% url language_select code=code p=q.p %}">{{ lang.language }}</a>
	</li>
	{% endif %}
{% endfor %}
{% endwith %}
</ul>

{% endblock %}
