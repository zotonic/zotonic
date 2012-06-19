{% extends "page.tpl" %}

{% block main %}
	{% inherit %}
	{% if not id.body %}
		{% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}

		{% include "_page_depiction.tpl" %}
		{% include "_page_thumbnails.tpl" %}
	{% endif %}
{% endblock %}

{% block depiction %}
{% if id.body %}
	{% inherit %}
{% endif %}
{% endblock %}

{% block thumbnails %}
{% if id.body %}
	{% inherit %}
{% endif %}
{% endblock %}

