{% extends "email_base.tpl" %}

{% block title %}{_ How to reset your password _} [{{ m.config.site.title.value }}]{% endblock %}

{% block body %}
<p>{_ Hello _} {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

<p>{_ You requested a new password for _} <a href="http://{{ m.site.hostname }}">{{ m.site.hostname }}</a>. {_ Below are your account details and a link to set a new password. _}</p>

<p>{_ Your account name is _} “<strong>{{ m.identity[id].username|escape }}</strong>”.  {_ The e-mail address associated with your account is _} “<strong>{{ email|default:(m.rsc[id].email)|escape }}</strong>”.</p>

<p>{_ Click on the link below to enter a new password, when clicking doesn't work then you can copy and paste the complete address to your browser. _}</p>

<p><a href="http://{{ m.site.hostname }}{% url password_reset secret=secret %}">http://{{ m.site.hostname }}{% url logon f="password_reset" secret=secret %}</a></p>

<p>{_ When you didn't request a password reset, you can ignore this e-mail. Maybe someone made an error typing his or her e-mail address. _}</p>

<p>{_ Thank you, _}</p>
<p>{_ The crew at _} {{ m.config.site.title.value }}.</p>

<p style="color: #666; font-size: 80%;">--<br/>
{_ You receive this e-mail because you or someone else requested a password reset for your account. You or the someone else either entered your username or e-mail address.  You will not receive any additional e-mails because of this request. _}</p>

{% endblock %}
