{% extends "collection.tpl" %}

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
							<li class="feature-item clearfix{% if forloop.last %} last-feature{% endif %}">
								<h2>{{ m.rsc[c_id].title }}</h2>
								{% if m.rsc[c_id].summary %}
									<p class="summary">{{ m.rsc[c_id].summary }}</p>
								{% endif %}
								{% if m.rsc[c_id].depiction %}
									{% image m.rsc[c_id].depiction width=450 lossless %}
								{% endif %}
							</li>
							{% endfor %}
						<ul>
					</div>
				</section>
			{% endif %}
		{% endfor %}
	</section>	

{% endblock %}
