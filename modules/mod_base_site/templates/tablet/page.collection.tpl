{% extends "page.tpl" %}

{% block main %}
	{% inherit %}
	{% if not id.body %}
		{% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}

		{% with id.o.hasdocument as xs %}
		{% with id.o.depiction as ds %}
		{% if xs or ds %}
		<ul class="thumbnails">
			{% for d in ds %}
				{% catinclude "_thumbnail_list_item.tpl" d %}
			{% endfor %}
			{% for d in xs %}
				{% catinclude "_thumbnail_list_item.tpl" d %}
			{% endfor %}
		</ul>
		{% endif %}
		{% endwith %}
		{% endwith %}
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

