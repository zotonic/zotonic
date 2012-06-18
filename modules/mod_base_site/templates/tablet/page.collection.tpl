{% extends "page.tpl" %}

{% block main %}
    {% inherit %}
    {% if not id.body %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id is_large %}
    {% endif %}
{% endblock %}

