{% extends "page.tpl" %}

{% block main %}
	{% inherit %}
	{% if not id.body %}
		{% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}
		{% if not id.blocks %}
		    {% include "_page_depiction.tpl" is_landscape %}
		{% endif %}
		{% include "_page_thumbnails.tpl" %}
	{% endif %}
{% endblock %}

{% block depiction %}
{% if id.body or id.blocks %}
	{% inherit %}
{% endif %}
{% endblock %}

{% block thumbnails %}
{% if id.body %}
	{% inherit %}
{% endif %}
{% endblock %}

