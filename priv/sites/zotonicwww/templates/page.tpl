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
				    {% if id.is_a.image or id.is_a.document %}
					    <a href="{% url media_inline id=id %}" title="{_ Click to download _}">{% image id width=540 height=340 %}</a>
					{% else %}
				        {% media id width=540 height=340 %}
					{% endif %}
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
    {% catinclude "_sidebar.tpl" id %}
</aside>
{% endblock %}
