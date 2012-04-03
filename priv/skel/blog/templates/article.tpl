{% extends "page.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block chapeau %}

	{% include "_article_chapeau.tpl" %}

{% endblock %}

{% block content %}

	<h1>{{ m.rsc[id].title }}</h1>

	{% if m.rsc[id].summary %}
	<p class="summary">
		{{ m.rsc[id].summary }}
	</p>
	{% endif %}

	{{ m.rsc[id].body|show_media }}

	{% include "_blocks.tpl" %}

	<section id="comments">{% include "_comments.tpl" id=id %}</section>
	{% include "_article_prevnext.tpl" id=id %}

{% endblock %}

{% block sidebar %}
	{% include "_article_sidebar.tpl" %}
{% endblock %}
