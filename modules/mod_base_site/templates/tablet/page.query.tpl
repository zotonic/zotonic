{% extends "page.tpl" %}

{% block main %}
{% include "_meta.tpl" %}

{% with m.search.paged[{query query_id=id pagelen=20 page=q.page}] as result%}
    {% include "_content_list.tpl" list=result %}
    {% pager id=id result=result in_collection=q.in_collection %}
{% endwith %}
{% endblock %}

{% block subnavbar %}
<div {% include "_language_attrs.tpl" id=id %}>

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

	{% block depiction %}
		{% with id.depiction as dep %}
		{% if dep and not dep.id.is_a.document %}
		<div class="thumbnail depiction">
			<img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" />
			{% if dep.id.summary %}
			<p class="caption"><span class="icon icon-camera"></span> <a href="{{ dep.id.page_url }}">{{ dep.id.summary }}</a></p>
			{% endif %}
		</div>
		{% endif %}
		{% endwith %}
	{% endblock %}
	{% include "_address.tpl" %}

	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}
	
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

{% catinclude "_subnavbar.tpl" id %}
&nbsp;
{% endblock %}
