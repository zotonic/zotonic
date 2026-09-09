{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your account _}{% endblock %}

{% block body %}
<p>{_ Dear _} {% include "_name.tpl" id=user_id %},</p>

<p>{_ Thank you for signing up. Please confirm your email address to activate your account. _}</p>

<p>{_ Please follow the link below. _}</p>

<p><a href="{% url signup_confirm key=verify_key absolute_url %}">{_ Confirm my account. _}</a></p>

<p>{_ Hope to see you soon. _}</p>
{% endblock %}
