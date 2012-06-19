{% with id.o.hasdocument as xs %}
{% with id.o.depiction as ds %}
{% if xs or ds|length > 1 %}
<ul class="thumbnails row-fluid">
	{% for d in ds %}
	{% if not forloop.first or d.is_a.document %}
		{% catinclude "_thumbnail_list_item.tpl" d %}
	{% endif %}
	{% endfor %}
	{% for d in xs %}
		{% catinclude "_thumbnail_list_item.tpl" d %}
	{% endfor %}
</ul>
{% endif %}
{% endwith %}
{% endwith %}
