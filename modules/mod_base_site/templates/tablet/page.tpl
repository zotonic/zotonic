{% extends "base_sidebar.tpl" %}

{# Page for TABLET+ #}

{% block main %}
<div {% include "_language_attrs.tpl" id=id %}>
    {% include "_title.tpl" %}
    {% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

    {% block depiction %}
        {% with id.depiction as dep %}
        {% if dep %}
        <div class="thumbnail depiction">
            <a href="{{ dep.id.page_url }}"><img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" /></a>
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
    {% block subnav %}{% include "_subnav.tpl" %}{% endblock %}
    &nbsp;
{% endblock %}

{% block sidebar %}
	{% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}
    {% include "_content_list.tpl" list=id.s.haspart title=_"More" %}
	{% include "_content_list.tpl" list=m.search[{latest cat=id.category_id pagelen=10}] title=_"Latest" exclude=[id] %}
{% endblock %}
