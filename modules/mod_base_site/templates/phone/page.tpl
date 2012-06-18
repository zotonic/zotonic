{% extends "base.tpl" %}

{# Simple page for TEXT (no media, as simple as possible) #}

{% block content %}
<div {% include "_language_attrs.tpl" id=id %}>
    {% include "_title.tpl" %}
    {% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

	{% include "_address.tpl" %}

    {% block subnav %}
        {% include "_subnav.tpl" %}
    {% endblock %}

    {% block body %}
	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}
    {% endblock %}

    {% block below_body %}{% endblock %}

    {% block related %}
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

    	{% with id.o.haspart, id.s.haspart as sub,super %}
    	{% if sub or super %}
        	<h3>{_ More _}</h3>
            {% include "_content_list.tpl" list=sub %}
            {% include "_content_list.tpl" list=super %}
        {% endif %}
        {% endwith %}
    {% endblock %}
</div>
{% endblock %}
