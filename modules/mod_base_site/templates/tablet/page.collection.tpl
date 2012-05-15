{% extends "page.tpl" %}

{% block main %}
    {% inherit %}
    {% include "_content_list.tpl" list=id.o.haspart in_collection=id %}
{% endblock %}
