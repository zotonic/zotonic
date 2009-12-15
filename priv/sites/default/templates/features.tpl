{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page features{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

	<article id="content">
		<div class="padding">
			<h1>{{ m.rsc[id].title }}</h1>
			{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}
			{{ m.rsc[id].body }}
		</div>
	</article>

	<section class="feature-wrapper clearfix">
		{% for c_ids in m.rsc[id].o.haspart|split_in:2 %}
			{% if c_ids %}
				<section class="feature-members zp-50">
					<div class="padding">
						<ul class="feature-list">
							{% for c_id in c_ids %}
							<li class="feature-item {% if forloop.last %}last-feature{% endif %}">
								{% if m.rsc[c_id].depiction %}
									{% image m.rsc[c_id].depiction width=70 height=70 crop %}
								{% endif %}
								<h2><a href="{{ m.rsc[c_id].page_url }}" title="{{ m.rsc[c_id].seo_description | default: m.rsc[c_id].summary }}">{{ m.rsc[c_id].title }}</a></h2>
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
	</section>
	
{% endblock %}