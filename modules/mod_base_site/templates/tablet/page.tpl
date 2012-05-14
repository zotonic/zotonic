{% extends "base_sidebar.tpl" %}

{# Page for TABLET+ #}

{% block main %}
<div {% include "_language_attrs.tpl" id=id %}>
    <h1>{{ m.rsc[id].title }}</h1>

    {% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

    {% block depiction %}
        {% with id.depiction as dep %}
        {% if dep %}
        <div class="thumbnail depiction">
            <img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" />
            {% if dep.id.summary %}<p class="caption">{{ dep.id.summary }}</p>{% endif %}
        </div>
        {% endif %}
        {% endwith %}
    {% endblock %}
	{% include "_address.tpl" %}

	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}

    {% with id.o.depiction as ds %}
    {% if ds|length > 1 %}
    <ul class="thumbnails">
        {% for d in ds %}
        {% if not forloop.first %}
        <li class="span3">
            <a href="{{ d.page_url }}" class="thumbnail"><img src="{% image_url d mediaclass="base-thumbnail" %}" alt="{{ d.title }}" title="{{d.title}}"/></a>
        </li>
        {% endif %}
        {% endfor %}
    </ul>
    {% endif %}
	{% endwith %}
</div>
{% endblock %}

{% block subnavbar %}
{% block subnav %}
    {% include "_subnav.tpl" %}
{% endblock %}
&nbsp;
{% endblock %}

{% block sidebar %}
    {% block related %}
    	{% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}

    	{% with id.o.haspart, id.s.haspart as sub,super %}
    	{% if sub or super %}
        	<h3>{_ Related _}</h3>
            {% include "_content_list.tpl" list=sub %}
            {% include "_content_list.tpl" list=super %}
        {% endif %}
        {% endwith %}
    {% endblock %}
{% endblock %}
