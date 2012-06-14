{% extends "page.tpl" %}

{% block body %}
    {% mailinglist_subscribe id=id %}
    {% inherit %}
{% endblock %}
