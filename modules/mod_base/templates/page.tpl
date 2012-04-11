{% extends "base.tpl" %}

{% block content %}
	<h1>{{ m.rsc[id].title }}</h1>

	{% if m.rsc[id].summary %}
		<p class="summary">{{ m.rsc[id].summary }}</p>
	{% endif %}

	{{ m.rsc[id].body|show_media }}
	
	{% include "_blocks.tpl" %}

	<section id="comments">{% include "_comments.tpl" id=id %}</section>
{% endblock %}

{% block sidebar %}
{% endblock %}