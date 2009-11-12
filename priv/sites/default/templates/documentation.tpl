{% extends "page.tpl" %}

{% block breadcrumb %}
	{% with m.rsc[id].category_id as category_id %}
		<p class="breadcrumb">
			<a href="{{ m.rsc.page_home.page_url }}">{{ m.rsc.page_home.short_title | default: m.rsc.page_home.title}}</a> &raquo;
			
			{% for cat_id in m.category[category_id].path %}
				{% ifnotequal m.rsc[cat_id].name "text" %}
					<a href="{{ m.rsc[cat_id].page_url }}">{{ m.rsc[cat_id].short_title | default: m.rsc[cat_id].title }}</a> &raquo;
				{% endifnotequal %}
			{% endfor %}
			<a href="{{ m.rsc[category_id].page_url }}">{{ m.rsc[category_id].short_title | default: m.rsc[category_id].title }}</a> &raquo;
			
			{{ m.rsc[id].short_title | default: m.rsc[id].title }}
		</p>
	{% endwith %}
{% endblock %}

{% block sidebar %}
	{% with m.rsc[id].category_id as category_id %}
		<aside id="sidebar" class="zp-33">
			<h2><a href="{{ m.rsc[category_id].page_url }}">{{ m.rsc[category_id].title }}</a></h2>
			
			{% ifequal m.rsc[category_id].name "zt_template_filter" %}
				{% for ids in m.search[{all_bytitle cat_is=category_id}]|vsplit_in:2 %}
				<div class="zp-50">
					<ul class="list-filters">
						{% for title,c_id in ids %}
							{% ifequal c_id id %}
							<li><a class="current" href="{{ m.rsc[c_id].page_url }}">{{ title }}</a></li>
							{% else %}
								<li><a href="{{ m.rsc[c_id].page_url }}">{{ title }}</a></li>
							{% endifequal %}
						{% endfor %}
					</ul>
				</div>
				{% endfor %}
			{% else %}
				<ul class="list-filters">
					{% for title,c_id in m.search[{all_bytitle_featured cat_is=category_id}] %}
						{% ifequal c_id id %}
							<li><a class="current" href="{{ m.rsc[c_id].page_url }}">{{ title }}</a></li>
						{% else %}
							<li><a href="{{ m.rsc[c_id].page_url }}">{{ title }}</a></li>
						{% endifequal %}
					{% endfor %}
				</ul>
			{% endifequal %}

			{% include "_documents.tpl" %}
		</aside>
	{% endwith %}
{% endblock %}	