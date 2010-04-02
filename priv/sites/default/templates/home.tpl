{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block content %}

	{% if m.rsc[id].is_featured %}
	
		<h1>{{ m.rsc[id].title }}</h1>
		<p class="summary">{{ m.rsc[id].summary }}</p>
	
		{% for id in m.rsc[id].media %}
			<figure class="image-wrapper block-level-image">
				{% media id width=445 crop class=align alt=m.rsc[id].title %}
				{% if m.rsc[id].summary %}<p class="image-caption">{{ m.rsc[id].summary }}</p>{% endif %}
			</figure>
		{% endfor %}

	{% endif %}

	{% with m.search.paged[{query cat='article' sort='-publication_start' page=q.page pagelen=m.config.site.pagelen.value}] as result %}

		{% for id in result %}
			
			{% if forloop.first %}

				{% include "_article_summary.tpl" id=id big=1 %}

			{% else %}
				
				{% include "_article_summary.tpl" id=id %}
			
			{% endif %}
			
		{% endfor %}
		
		{% pager result=result %}

	{% endwith %}

{% endblock %}
