{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page category{% endblock %}

{% block banner %}{% endblock %}

{% block content %}

    <div class="zp-100 clearfix">
	<article id="content" class="zp-67">
		<div class="padding">
			{% with m.category[id].path as path %}
				{% ifnotequal path|length 1 %}
					<p class="breadcrumb">
						<a href="{{ m.rsc.page_home.page_url }}">{{ m.rsc.page_home.short_title | default: m.rsc.page_home.title}}</a> &raquo;
						{% for cat_id in path %}
							{% ifnotequal m.rsc[cat_id].name "text" %}
								<a href="{{ m.rsc[cat_id].page_url }}">{{ m.rsc[cat_id].short_title | default: m.rsc[cat_id].title }}</a> &raquo;
							{% endifnotequal %}
						{% endfor %}
						
						{{ m.rsc[id].short_title | default: m.rsc[id].title }}
						
					</p>
				{% endifnotequal %}
			{% endwith %}

			<h1>{{ m.rsc[id].title }}</h1>
			{% if m.rsc[id].summary %}<p class="summary">{{ m.rsc[id].summary }}</p>{% endif %}
			{{ m.rsc[id].body }}

		</div>
	</article>
    <section class="zp-33 clearfix">
            {% include "_block_search.tpl" %}
    </section>
    </div>	

    <section class="feature-wrapper clearfix zp-100">
	{% with m.category[id].tree1 as sub_cats %}
		{% with m.search[{all_bytitle_featured cat_is=id}] as c_ids %}

		{% if sub_cats %}

			{% for cs in sub_cats|vsplit_in:3 %}
			<section class="feature-members zp-33">
				<div class="padding">
					<ul class="feature-list">
						{% for cat in cs %}
							{% with cat.id as c_id %}
							<li class="feature-item">
								<h2><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h2>
								<p class="summary">{{ m.rsc[c_id].summary }}</p>
							</li>
							{% endwith %}
						{% endfor %}
					</ul>
				</div>
			</section>
            {% endfor %}
{#
			{% if c_ids %}
				<section class="collection-members zp-33">
					<div class="padding">
						<ul class="item-list">
						{% for title,c_id in c_ids %}
							<li class="list-item">
								<h3><a href="{{ m.rsc[c_id].page_url }}">{{ title }}</a></h3>
								<p class="summary">{{ m.rsc[c_id].summary }}</p>
							</li>
						{% endfor %}
						</ul>
					</div>
				</section>
			{% endif %}
#}

		{% else %}

			{% for ids in c_ids|vsplit_in:3 %}
				{% if ids %}
					<section class="feature-members zp-33">
						<div class="padding">
							<ul class="feature-list">
								{% for title,c_id in ids %}
								<li class="feature-item">
									<h2><a href="{{ m.rsc[c_id].page_url }}">{{ title }}</a></h2>
									{% if m.rsc[c_id].summary %}
										<p class="summary">{{ m.rsc[c_id].summary | truncate:80 }}</p>
									{% endif %}
								</li>
								{% endfor %}
							</ul>
						</div>
					</section>
				{% endif %}
			{% endfor %}

		{% endif %}
		{% endwith %}
	{% endwith %}
    </section>
{% endblock %}
