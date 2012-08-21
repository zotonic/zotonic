{% extends "base.tpl" %}

{# Simple page for TEXT (no media, as simple as possible) #}

{% block content %}
<div {% block content_attributes %}{% include "_language_attrs.tpl" id=id %}{% endblock %}>
	{% include "_title.tpl" %}
	{% block main %}
		{% include "_meta.tpl" %}

		{% if m.rsc[id].summary %}
			<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
		{% endif %}

		{% include "_address.tpl" %}

		{% block subnav %}
			{% include "_subnav.tpl" %}
		{% endblock %}

		{% block body %}
		<div class="body">
			{{ m.rsc[id].body }}
			{% include "_blocks.tpl" %}
		</div>
		{% endblock %}

		{% block below_body %}{% endblock %}

		{% block seealso %}
			{% include "_content_list.tpl" list=id.o.hasfeatured %}
	        {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
	        {% include "_content_list.tpl" list=id.o.relation %}
		{% endblock %}
		
		{% block thumbnails %}
		    {% include "_page_thumbnails.tpl" %}
		{% endblock %}

		{% block sidebar_collection %}
			{% with m.rsc.sidebar_collection.id as id %}
			{% include "_content_list.tpl" list=id.o.haspart %}
			{% endwith %}
		{% endblock %}
	{% endblock %}
</div>
{% endblock %}
