{% extends "base.tpl" %}

{# Page for TABLET+ #}

{% block main %}
<div {% include "_language_attrs.tpl" id=id %}>
	{% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

	{% block depiction %}
		{% with id.depiction as dep %}
		{% if dep and not dep.id.is_a.document %}
		<div class="thumbnail depiction {% if 10*dep.width / dep.height > 8 %}landscape{% else %}portrait{% endif %}">
			<img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" />
			{% if dep.id.summary %}
			<p class="caption"><span class="icon icon-camera"></span> <a href="{{ dep.id.page_url }}">{{ dep.id.summary }}</a></p>
			{% endif %}
		</div>
		{% endif %}
		{% endwith %}
	{% endblock %}
	{% include "_address.tpl" %}

    {% block body %}
	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}
	{% endblock %}
	
	{% with id.o.hasdocument as xs %}
	{% with id.o.depiction as ds %}
	{% if xs or ds|length > 1 %}
	<ul class="thumbnails">
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
	
	{% block below_body %}
	{% endblock %}
</div>
{% endblock %}

{% block subnavbar %}
	{% catinclude "_subnavbar.tpl" id %}
	&nbsp;
{% endblock %}

