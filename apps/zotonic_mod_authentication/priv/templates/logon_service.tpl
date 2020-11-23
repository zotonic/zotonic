{% extends "base_simple.tpl" %}

{# Template shown in popup window when redirecting to an external identity service #}

{% block title %}{_ Authorizing... _}{% endblock %}

{% block html_attr %}data-onauth="#"{% endblock %}

{% block content %}
    <h2>{_ One moment please _}</h2>

    {% if service_name %}
        <p>{_ Redirecting to _} {{ service_name }}...</p>
    {% else %}
        <p>{_ Redirecting... _}</p>
    {% endif %}
{% endblock %}
