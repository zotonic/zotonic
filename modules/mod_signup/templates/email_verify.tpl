{% extends "email_base.tpl" %}

{% block title %}Please confirm your account.{% endblock %}

{% block body %}
<p>Dear {{ m.rsc[user_id].title|default:m.rsc[user_id].name_first }},</p>

<p>Thank you for registering at our site. We request you to confirm your account before you can use it.</p>

<p>Please follow the link below.</p>

<p><a href="{% url signup_confirm key=verify_key %}">Confirm my account.</a></p>

<p>If the link does not work then you can go to <a href="http://{{ m.site.hostname }}{% url signup_confirm %}">http://{{ m.site.hostname }}{% url signup_confirm %}</a> and enter the key <strong>{{ verify_key }}</strong> in the input field.</p>

<p>Hope to see you soon.</p>

{% endblock %}
