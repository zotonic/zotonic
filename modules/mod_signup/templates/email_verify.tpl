{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your account. _}{% endblock %}

{% block body %}
<p>{_ Dear _} {{ m.rsc[user_id].title|default:m.rsc[user_id].name_first }},</p>

<p>{_ Thank you for registering at our site. We request you to confirm your account before you can use it. _}</p>

<p>{_ Please follow the link below. _}</p>

<p><a href="{% url signup_confirm key=verify_key %}">{_ Confirm my account. _}</a></p>

<p>{_ If the link does not work then you can go to _} <a href="http://{{ m.site.hostname }}{% url signup_confirm %}">http://{{ m.site.hostname }}{% url signup_confirm %}</a> {_ and enter the key _} <strong>{{ verify_key }}</strong> {_ in the input field. _}</p>

<p>{_ Hope to see you soon. _}</p>

{% endblock %}
