{% extends "email_base.tpl" %}

{% block title %}{_ Your one-time code is: _} {{ code }}{% endblock %}

{% block body %}
<p>{_ Thank you for signing up. _}</p>

<p>{_ This is your one-time code to continue: _}</p>

<p style="font-size: 1.5em; font-weight: bold; text-align: center; margin: 20px 0;">
    {{ code }}
</p>

<p>{_ Hope to see you soon. _}</p>
{% endblock %}
