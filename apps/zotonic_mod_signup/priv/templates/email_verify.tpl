{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your account _}{% endblock %}

{% block body %}
<p>{_ Dear _} {{ m.rsc[user_id].title|default:m.rsc[user_id].name_first }},</p>

<p>{_ Thank you for registering at our site. We request you to confirm your account before you can use it. _}</p>

<p>{_ Please follow the link below. _}</p>

<p><a href="{% url signup_confirm key=verify_key absolute_url %}">{_ Confirm my account. _}</a></p>

<p>{_ Hope to see you soon. _}</p>
{% endblock %}
