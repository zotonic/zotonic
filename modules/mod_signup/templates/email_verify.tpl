{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your account _}{% endblock %}

{% block body %}
<p>{_ Dear _} {{ m.rsc[user_id].title|default:m.rsc[user_id].name_first }},</p>

<p>{_ Thank you for registering at our site. We request you to confirm your account before you can use it. _}</p>

<p>{_ Please follow the link below. _}</p>

<p><a href="{% url signup_confirm key=verify_key absolute_url %}">{_ Confirm my account. _}</a></p>

<p>{_ If the link does not work then you can go to _} <a href="{% url signup_confirm absolute_url %}">{% url signup_confirm absolute_url %}</a> {_ and enter the key _} <strong>{{ verify_key }}</strong> {_ in the input field. _} {_ Hope to see you soon. _}</p>
{% endblock %}
