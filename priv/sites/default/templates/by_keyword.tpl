{% extends "base.tpl" %}

{% block title %}Articles for "{{ m.rsc[q.id].title }}"{% endblock %}

{% block chapeau %}
	<h5 class="chapeau">Articles about: {{ m.rsc[q.id].title }}</h5>
{% endblock %}

{% block content %}

	{% with m.search.paged[{referrers id=q.id}] as result %}
	
		{% for id, predicate in result %}
			{% include "_article_summary.tpl" id=id %}
		{% endfor %}
		
		{% pager result=result dispatch='archives_y' year=q.year %}
	
	{% endwith %}

{% endblock %}

{% block sidebar %}
	{% include "_sidebar.tpl" show_cloud=1 %}
{% endblock %}
