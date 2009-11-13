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
					<li style="margin: 0 8px; padding: 0 8px">
						<div class="zp-67">
							<a href="{{ m.rsc[c_id].page_url }}">{% image m.rsc[c_id].depiction width=380 height=240 crop style="margin: 8px 0" %}</a>
						</div>
						
						<div class="zp-33">
							<h2><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h2>

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
