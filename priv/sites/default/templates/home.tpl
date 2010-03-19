{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}

	{% with m.search.paged[{query cat='article' sort='-publication_start' page=q.page pagelen=m.config.site.pagelen.value}] as result %}

		{% for id in result %}
			{% include "_article_summary.tpl" id=id %}
		{% endfor %}
		
		{% pager result=result %}

	{% endwith %}

{% endblock %}
