{% extends "base.tpl" %}

{% block title %}{{ m.rsc[id].seo_title | default: m.rsc[id].title }}{% endblock %}

{% block page_class %}page{% endblock %}

{% block content %}

	<article id="content" class="{% block content_class %}zp-67{% endblock %}">
		<div class="padding">
			{% block breadcrumb %}{% endblock %}
			{% block pagetitle %}<h1>{{ m.rsc[id].title }}</h1>{% endblock %}

            {% if id|is_a:"article" %}
            {% include "_blog_meta.tpl" %}
            {% endif %}

			{% if m.rsc[id].website %}
			<p class="website"><a href="{{ m.rsc[id].website }}">{{ m.rsc[id].website }}</a></p>
			{% endif %}

			<p class="summary">{{ m.rsc[id].summary }}</p>

			{% with m.rsc[id].media|without_embedded_media:id as media %}
				{% if media[1] %}
					<a href="{% url media_inline id=media[1] %}" title="{_ Click to download _}">{% media media[1] width=540 height=340 %}</a>
				{% else %}
					{% media id width=540 height=340 %}
				{% endif %}
			{% endwith %}

            {{ m.rsc[id].body|show_media }}
		</div>

        {% block below_content %}
        {% endblock %}

        <section id="comments">
            {% include "_comments.tpl" id=id %}
        </section>

	</article>

{% endblock %}

{% block sidebar %}

	<aside id="sidebar" class="zp-33">
        {% include "_keywords.tpl" %}

		{% with m.rsc[id].s.haspart as collections %}
			{% if collections %}
				{% for c_id in collections %}
					<h2><a href="{{ m.rsc[c_id].page_url }}">{{ m.rsc[c_id].title }}</a></h2>

					<ul class="item-list">
					{% for p_id in m.rsc[c_id].o.haspart %}
						<li class="list-item">
							{% ifnotequal p_id id %}
								<h3><a href="{{ m.rsc[p_id].page_url }}">{{ m.rsc[p_id].title }}</a></h3>
							{% else %}
								<h3>{{ m.rsc[p_id].title }}</h3>
							{% endifnotequal %}
							{% if m.rsc[p_id].summary %}
								<p class="summary">{{ m.rsc[p_id].summary }}</p>
							{% endif %}
						</li>
					{% endfor %}
					</ul>
				{% endfor %}
			{% else %}
			{% endif %}
		{% endwith %}

		{% include "_documents.tpl" %}
	</aside>

{% endblock %}
