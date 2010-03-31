{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page collection{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

	<article id="content" class="zp-67">
		<div class="padding">
			
			{% with m.rsc[id].s.hassection as p %}
				{% with p[1].hassection as siblings %}
				<p>
					<a href="{{ p[1].page_url }}">{{ p[1].title }}</a>
					
					{% with siblings|before:id as s_id %}
						{% if s_id %}&sdot; <a href="{{ m.rsc[s_id].page_url }}">&laquo; {{ m.rsc[s_id].title }}</a>{% endif %}
					{% endwith %}
					{% with siblings|after:id as s_id %}
						{% if s_id %}&sdot; <a href="{{ m.rsc[s_id].page_url }}">{{ m.rsc[s_id].title }} &raquo;</a>{% endif %}
					{% endwith %}
				</p>
				{% endwith %}
			{% endwith %}
			
			<h1>{{ m.rsc[id].title }}</h1>
			{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}
			{{ m.rsc[id].body }}
		</div>
	</article>

	<aside class="collection-members zp-33">
		<div class="padding">
			
			{% with m.rsc[id].o.hassection as sections %}
				{% if sections %}
					<ul class="item-list">
						{% for c_id in sections %}
						<li class="list-item">
							<h3><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h3>
							{% if m.rsc[c_id].summary %}
								<p class="summary">{{ m.rsc[c_id].summary }}</p>
							{% endif %}
						</li>
						{% endfor %}
					</ul>
				{% endif %}
			{% endwith %}

			{% include "_documents.tpl" %}
			
			<p class="zotonic-license-notice">
				This page is part of the Zotonic documentation, which is licensed under the <a href="http://www.apache.org/licenses/LICENSE-2.0.html">Apache License version 2.0</a>.
			</p>
			
		</div>
	</aside>

{% endblock %}

{% block sidebar %}{% endblock %}