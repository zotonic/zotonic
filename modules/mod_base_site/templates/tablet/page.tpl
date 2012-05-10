{% extends "base_sidebar.tpl" %}

{# Page for PHONE+ #}

{% block above %}
<div class="row-fluid">
    <div class="span-12">
        <h1>{{ m.rsc[id].title }}</h1>
    </div>
</div>
{% endblock %}

{% block main %}
<div {% include "_language_attrs.tpl" id=id %}>
    {% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

    {% block depiction %}
        {% with id.depiction as dep %}
        {% if dep %}
        <div class="thumbnail depiction">
            <img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" />
            <p>{{ dep.id.summary|default:dep.id.title }}</p>
        </div>
        {% endif %}
        {% endwith %}
    {% endblock %}
	{% include "_address.tpl" %}

    {% block subnav %}
        {% include "_subnav.tpl" %}
    {% endblock %}

	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}
</div>
{% endblock %}

{% block sidebar %}
    {% block related %}
    	{% include "_content_list.tpl" list=id.o.hasdocument title=_"Documents"%}
    	{% include "_content_list.tpl" list=id.o.depiction title=_"Media"%}

    	{% with id.o.haspart, id.s.haspart as sub,super %}
    	{% if sub or super %}
        	<h3>{_ Related _}</h3>
            {% include "_content_list.tpl" list=sub %}
            {% include "_content_list.tpl" list=super %}
        {% endif %}
        {% endwith %}
    {% endblock %}
{% endblock %}
