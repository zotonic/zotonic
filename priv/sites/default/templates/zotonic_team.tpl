{% extends "collection.tpl" %}

{% block content %}

	<article class="zp-33">
		<div class="padding">
			<h1>{{ m.rsc[id].title }}</h1>
			{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}
			{{ m.rsc[id].body }}
		</div>
	</article>

	<section class="zp-67">
		<div class="padding">
			<ul class="item-list">
				{% for c_id in m.rsc[id].o.haspart %}
					<li style="margin: 0 8px; padding: 0 8px; border-bottom: 1px dotted #666;">
						<div class="zp-33">
							{% image m.rsc[c_id].depiction width=180 height=220 crop style="margin: 8px 0" %}
						</div>
						
						<div class="zp-67">
							<h2>{{ m.rsc[c_id].title }}</h2>

							{% if m.rsc[c_id].summary %}
								<p class="summary">{{ m.rsc[c_id].summary }}</p>
							{% endif %}
							
							{{ m.rsc[c_id].body }}
						</div>
						<div class="clear"></div>
					</li>
				{% endfor %}
			</ul>
		</div>
	</section>

{% endblock %}
