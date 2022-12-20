{% extends "email_base.tpl" %}

{% block title %}
    {_ Your login details for _} {{ m.site.title|default:m.site.hostname }}
{% endblock %}

{% block body %}
    <p>{_ Hi _} {% include "_name.tpl" id=id %},</p>

    {% if is_allowed_use_mod_admin %}
        <p>{_ You have been granted access to _} {{ m.site.title|default:m.site.hostname }}.
        {_ You can now login to the admin, on the following URL: _}</p>
        <p><a href="{% url admin absolute_url %}">{% url admin absolute_url %}</a></p>
    {% else %}
        <p>{_ You have been granted access to _} {{ m.site.title|default:m.site.hostname }}.
        {_ You can now login on the following URL: _}</p>
        <p><a href="{% url logon absolute_url %}">{% url logon absolute_url %}</a></p>
    {% endif %}

    <p>{_ Your username is: _} <tt>{{ username }}</tt></p>

    <p>{_ If you don’t know your password, click on <i>Forgot your password?</i> at the login. _}</p>
{% endblock %}
