{% extends "page.tpl" %}

{% block main %}
	{% inherit %}
	{% if not (id.body or id.blocks) %}
		{% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}

		{% include "_page_depiction.tpl" is_landscape %}
		{% include "_page_thumbnails.tpl" %}
	{% endif %}
{% endblock %}

{% block depiction %}
{% if id.body or id.blocks %}
	{% inherit %}
{% endif %}
{% endblock %}

{% block thumbnails %}
{% if id.body or id.blocks %}
	{% inherit %}
{% endif %}
{% endblock %}

