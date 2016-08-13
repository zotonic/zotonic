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

    {% if id|is_a:"video" %}
        <!-- 16:9 aspect ratio -->
        <div class="embed-responsive embed-responsive-16by9">
            {% media id %}
        </div>
    {% endif %}
    {% with id.medium as medium %}
        {% if medium.filename %}
            {% include "_media_info.tpl" %}
        {% elseif medium.video_embed_code %}
            <h2>Embed code</h2>
            <pre>{{ medium.video_embed_code|force_escape|linebreaksbr }}</pre>
        {% endif %}
    {% endwith %}


    {% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}
    {% include "_content_list.tpl" list=id.o.relation is_large %}
{% endblock %}

{% block subnavbar %}
    {% inherit %}
    {% include "_content_list.tpl" list=id.s.depiction %}
    {% include "_content_list.tpl" list=id.s.hasdocument %}
{% endblock %}
