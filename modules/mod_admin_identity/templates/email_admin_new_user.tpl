{% extends "email_base.tpl" %}

{% block title %}
    {_ Your login details for _} {{ m.site.title }}
{% endblock %}

{% block body %}
    <p>{_ Hi _}, {{ id.title }}!</p>

    <p>{_ You have been granted access to _} {{ m.site.title }}. {_ You can now login to the admin, on the following URL: _}</p>

    <p><a href="{% url admin use_absolute_url %}">{% url admin use_absolute_url %}</a></p>

    <p>
        {_ Your user details are: _}
    </p>

    <p>
        {_ Username _}: <tt>{{ username }}</tt><br />
        {_ Password _}: <tt>{{ password }}</tt>
    </p>
{% endblock %}
