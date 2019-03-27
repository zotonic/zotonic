{% extends "email_base.tpl" %}

{% block title %}{_ How to reset your password _}{% endblock %}

{% block body %}
{% if not id or not username %}
    <p>{_ Hello _},</p>

    <p>{_ You've requested a new password for _} <a href="https://{{ m.site.hostname }}/">{{ m.site.hostname }}</a>.</p>

    <p>{_ However this email address does not belong to one of our registered users so you will not be able to change the password. _}</p>

    <p>{_ If you think you have an account and were expecting this email, please try again using the email address you gave when signing up. _}</p>
{% else %}
    <p>{_ Hello _} {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

    <p>{_ You've requested a new password for _} <a href="https://{{ m.site.hostname }}/">{{ m.site.hostname }}</a>. {_ Below are your account details and a link to set a new password. _}</p>

    <p>{_ Your account name is _} “<strong>{{ username|escape }}</strong>”.{% if username != email|default:(m.rsc[id].email_raw) %} {_ The email address associated with your account is _} “<strong>{{ email|default:(m.rsc[id].email_raw)|escape }}</strong>”.{% endif %}</p>

    {% all include "_logon_extra_email_reset.tpl" identity_types=m.identity[id].all_types %}

    <p>{_ Click on the link below to enter a new password. When clicking doesn't work, please copy and paste the whole link. _}</p>

    <p><a href="{% url logon_reset secret=secret u=username absolute_url %}">{% url logon_reset u=username secret=secret absolute_url %}</a></p>
{% endif %}

<p>{_ If you didn't request a password reset, you can safely ignore this email. Maybe someone made an error typing his or her email address. _}</p>

{% endblock %}

{% block disclaimer %}
<p style="color: #666; font-size: 80%;">--<br/>
{_ You receive this email because you or someone else requested a password reset for your account. You or the someone else either entered your username or email address. You will not receive any additional emails because of this request. _}</p>
{% endblock %}
