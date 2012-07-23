{% with id.o.hasdocument as xs %}
{% with id.o.depiction as ds %}
{% if xs or ds %}
<ul class="thumbnails row-fluid">
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
