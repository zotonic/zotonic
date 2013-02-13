{% extends "email_base.tpl" %}

{% block title %}{_ Please verify your e-mail address _} [{{ m.config.site.title.value }}]{% endblock %}

{% block body %}
<p>{_ Hello _} {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

<p>{_ This e-mail address was added to your personal information on _} {{ m.config.site.title.value }}: {{ idn.key }}.</p>

{% with m.identity[id].username as username %}
{% if username %}
<p>{_ Your account name is _} “<strong>{{ username|escape }}</strong>”.</p>
{% endif %}
{% endwith %}

<p>{_ Click on the link below to confirm that this e-mail address is correct, when clicking doesn't work then you can copy and paste the complete address to your browser. _}</p>

<p><a href="{{ m.site.protocol }}://{{ m.site.hostname }}{% url identity_verify idn_id=idn.id verify_key=verify_key %}">{{ m.site.protocol }}://{{ m.site.hostname }}{% url identity_verify idn_id=idn.id verify_key=verify_key %}</a></p>

<p>{_ If you don't know this site then you can ignore this e-mail. Maybe someone made an error typing his or her e-mail address. _}</p>

<p>{_ Thank you, _}</p>
<p>{_ The crew at _} {{ m.config.site.title.value }}.</p>

<p style="color: #666; font-size: 80%;">--<br/>
{_ You receive this e-mail because you or someone else requested verification of your e-mail address. You or the someone else either entered your e-mail address. You will not receive any additional e-mails because of this request. _}</p>

{% endblock %}
