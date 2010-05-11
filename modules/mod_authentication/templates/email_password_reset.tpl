{% extends "email_base.tpl" %}

{% block title %}How to reset your password [{{ m.config.site.title.value }}]{% endblock %}

{% block body %}
<p>Hello {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

<p>You requested a new password for <a href="http://{{ m.site.hostname }}">{{ m.site.hostname }}</a>. Below are your account details and a link to set a new password.</p>

<p>Your account name is “<strong>{{ account_name|escape }}</strong>”.  The e-mail address associated with your account is “<strong>{{ email|escape }}</strong>”.</p>

<p>Click on the link below to enter a new password, when clicking doesn't work then you can copy and paste the complete address to your browser.</p>

<p><a href="http://{{ m.site.hostname }}{% url password_reset secret=secret %}">http://{{ m.site.hostname }}{% url password_reset secret=secret %}</a></p>

<p>When you didn't request a passsword reset then you can ignore this e-mail.  Maybe someone made an error typing his or her e-mail address.</p>

<p>Thank you,</p>
<p>The crew at {{ m.config.site.title.value }}.</p>

<p style="color: #666; font-size: 80%;">--<br/>
You receive this e-mail because you or someone else requested a password reset for your account. You or the someone else either entered your username or e-mail address.  You will not receive any additional e-mails because of this request.</p>

{% endblock %}