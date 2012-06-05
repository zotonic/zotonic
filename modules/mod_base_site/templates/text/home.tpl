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

    {% with id.o.haspart as haspart %}
    {% if haspart %}
        {% include "_content_list.tpl" list=haspart %}
        {% include "_content_list.tpl" list=m.search[{featured cat='text' pagelen=10}] %}
    {% else %}
        {% include "_content_list.tpl" list=m.search[{featured cat='text' pagelen=20}] %}
    {% endif %}
    {% endwith %}
{% endwith %}

{% endblock %}
