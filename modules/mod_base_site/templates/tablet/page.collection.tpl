{% extends "page.tpl" %}

{% block main %}
    {% inherit %}
    {% if not id.body %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
    {% endif %}
{% endblock %}

{% block subnavbar %}
    {% include "_subnav.tpl" %}

    {% if id.body %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
    {% endif %}

    &nbsp;
{% endblock %}
