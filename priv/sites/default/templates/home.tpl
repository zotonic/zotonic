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

	{% with m.search[{query cat='article' sort='-publication_start' pagelen=2}] as result %}

		<div id="results">
		{% for id in result %}
			{% include "_article_summary.tpl" id=id big=forloop.first %}
		{% endfor %}
		</div>

		{% wire id="more" action={moreresults result=result target="results" template="_article_summary.tpl"} %}
		<p><a href="javascript:;" id="more">Read more...</a></p>
	{% endwith %}

{% endblock %}
