{% extends "base.tpl" %}

{# Simple page for TEXT (no media, as simple as possible) #}

{% block content %}
<div {% include "_language_attrs.tpl" id=id %}>
	<h1>{{ m.rsc[id].title }}</h1>

    {% include "_meta.tpl" %}

	{% if m.rsc[id].summary %}
		<p class="summary"><b>{{ m.rsc[id].summary }}</b></p>
	{% endif %}

	{% include "_address.tpl" %}

    {% block subnav %}
        {% include "_subnav.tpl" %}
    {% endblock %}

	{{ m.rsc[id].body }}
	{% include "_blocks.tpl" %}

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
</div>
{% endblock %}
