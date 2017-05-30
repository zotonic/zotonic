{% extends "email_base.tpl" %}

{% block title %}{_ How to reset your password _}{% endblock %}

{% block body %}
<p>{_ Hello _} {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

<p>{_ You've requested a new password for _} <a href="{{ m.site.protocol }}://{{ m.site.hostname }}/">{{ m.site.hostname }}</a>. {_ Below are your account details and a link to set a new password. _}</p>

<p>{_ Your account name is _} “<strong>{{ m.identity[id].username|escape }}</strong>”.{% if m.identity[id].username != email|default:(m.rsc[id].email_raw) %} {_ The email address associated with your account is _} “<strong>{{ email|default:(m.rsc[id].email_raw)|escape }}</strong>”.{% endif %}</p>

{% all include "_logon_extra_email_reset.tpl" identity_types=m.identity[id].all_types %}

<p>{_ Click on the link below to enter a new password, when clicking doesn't work then you can copy and paste the complete address to your browser. _}</p>

<p><a href="{% url logon_reset secret=secret absolute_url %}">{% url logon_reset secret=secret absolute_url %}</a></p>

<p>{_ When you didn't request a password reset, you can ignore this email. Maybe someone made an error typing his or her email address. _}</p>
{% endblock %}

{% block disclaimer %}
<p style="color: #666; font-size: 80%;">--<br/>
{_ You receive this email because you or someone else requested a password reset for your account. You or the someone else either entered your username or email address. You will not receive any additional emails because of this request. _}</p>
{% endblock %}
