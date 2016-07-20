{% extends "page.tpl" %}

{% block main %}
    {% include "_meta.tpl" %}

    {% if m.rsc[id].summary %}
    	<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
    {% endif %}

    {% include "_address.tpl" %}

    <div class="body">
	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}
    </div>
    
    {% with id.o.depiction as ds %}
    {% if ds %}
    <ul class="thumbnails">
        {% for d in ds %}
        <li class="col-lg-3 col-md-3">
            <a href="{{ d.page_url }}" class="thumbnail"><img src="{% image_url d mediaclass="base-thumbnail" %}" alt="{{ d.title }}" title="{{d.title}}"/></a>
        </li>
        {% endfor %}
    </ul>
    {% endif %}
    {% endwith %}

    {% if id.is_a.document or id.is_a.image %}
        <p class="thumbnail">
            <a href="{% url media_inline id=id %}" title="{_ Click to download _}"><img src="{% image_url id mediaclass="base-media-preview" %}" alt="{_ Preview _}"/></a>
        </p>
        {% include "_media_info.tpl" %}
    {% else %}
        {% media id %}
    {% endif %}

    {% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}
    {% include "_content_list.tpl" list=id.o.relation is_large %}
{% endblock %}

{% block subnavbar %}
    {% inherit %}
    {% include "_content_list.tpl" list=id.s.depiction %}
    {% include "_content_list.tpl" list=id.s.hasdocument %}
{% endblock %}
