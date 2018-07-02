{% extends "base.tpl" %}

{% block title %}{_ Welcome _}{% endblock %}

{% block content %}
{% with m.rsc.page_home.id as id %}
    {% if id %}
        {% if id.summary %}
            <p class="summary">{{ id.summary }}</p>
        {% endif %}
        {{ id.body }}
    {% endif %}
    
    {% include "_content_list.tpl" list=id.o.hasfeatured %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
{% endwith %}

{% endblock %}
