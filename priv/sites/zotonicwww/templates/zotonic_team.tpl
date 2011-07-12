{% extends "collection.tpl" %}

{% block content %}

	<article class="zp-33" id="content">
		<div class="padding">
			<h1>{{ m.rsc[id].title }}</h1>
			{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}
			{{ m.rsc[id].body }}
		</div>
	</article>

	<section class="zp-67">
		<ul class="item-list">
			{% for c_id in m.rsc[id].o.haspart %}
				<li class="clearfix" style="margin: 0 0 18px 0; padding: 0 0 14px 0; border-bottom: 1px dotted #666;">
					<div class="zp-33">
						{% image m.rsc[c_id].depiction width=180 height=220 crop style="margin: 6px 0 0 0" %}
					</div>
					
					<div class="zp-67">
						<h2><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h2>
						{% if m.rsc[c_id].summary %}
							<p class="summary">{{ m.rsc[c_id].summary }}</p>
						{% endif %}

						{{ m.rsc[c_id].body }}
					</div>
				</li>
			{% endfor %}
		</ul>
	</section>

{% endblock %}
