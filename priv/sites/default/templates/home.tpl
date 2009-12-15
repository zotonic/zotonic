{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block featured %}

	<div class="featured">
	    <h1>{{ m.site.title }}</h1>
	    <p>{{ m.site.subtitle }}</p>
	</div>

{% endblock %}

{% block content %}

	{% with m.search.paged[{query cat='article' sort='-publication_start' page=q.page pagelen=m.config.site.pagelen.value}] as result %}
		{% for id in result %}
			{% include "_article_summary.tpl" id=id %}
		{% endfor %}
			{% pager result=result dispatch='home' %}
	{% endwith %}

{% endblock %}
