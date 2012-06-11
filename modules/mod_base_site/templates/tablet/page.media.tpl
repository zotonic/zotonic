{% extends "page.tpl" %}

{% block main %}
    {% include "_meta.tpl" %}

    {% if m.rsc[id].summary %}
    	<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
    {% endif %}

    {% include "_address.tpl" %}

	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}

    {% with id.o.depiction as ds %}
    {% if ds %}
    <ul class="thumbnails">
        {% for d in ds %}
        <li class="span3">
            <a href="{{ d.page_url }}" class="thumbnail"><img src="{% image_url d mediaclass="base-thumbnail" %}" alt="{{ d.title }}" title="{{d.title}}"/></a>
        </li>
        {% endfor %}
    </ul>
    {% endif %}
    {% endwith %}

    <p class="thumbnail">
        <a href="{% url media_inline id=id %}" title="{_ Click to download _}"><img src="{% image_url id mediaclass="base-media-preview" %}" alt="{_ Preview _}"/></a>
    </p>

    {% include "_media_info.tpl" %}
{% endblock %}

{% block subnavbar %}
    {% include "_content_list.tpl" list=id.s.depiction %}
    {% include "_content_list.tpl" list=id.s.hasdocument %}

    {% inherit %}
{% endblock %}