{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page collection{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

	<article id="content" class="zp-33">
		<div class="padding">
			<h1>{{ m.rsc[id].title }}</h1>
			{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}
			{{ m.rsc[id].body }}
		</div>
	</article>

	{% for c_ids in m.rsc[id].o.haspart|split_in:2 %}
		{% if c_ids %}
			<section class="collection-members zp-33">
				<div class="padding">
					<ul class="item-list">
						{% for c_id in c_ids %}
						<li class="list-item">
							<h3><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h3>
							{% if m.rsc[c_id].summary %}
								<p class="summary">{{ m.rsc[c_id].summary }}</p>
							{% endif %}
						</li>
						{% endfor %}
					</ul>
				</div>
			</section>
		{% endif %}
	{% endfor %}

{% endblock %}

{% block sidebar %}{% endblock %}