{% extends "base.tpl" %}

{# Page for TABLET+ #}

{% block main %}
<div {% include "_language_attrs.tpl" id=id %}>
	{% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

	{% block depiction %}
	    {% include "_page_depiction.tpl" %}
	{% endblock %}
	{% include "_address.tpl" %}

    {% block body %}
	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}
	{% endblock %}
	
	{% block thumbnails %}
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
	{% endblock %}
	
	{% block below_body %}
	{% endblock %}
</div>
{% endblock %}

{% block subnavbar %}
	{% catinclude "_subnavbar.tpl" id %}
	&nbsp;
{% endblock %}

