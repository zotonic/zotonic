{% extends "email_base.tpl" %}

{% block title %}{_ Please verify your email address _} [{{ m.site.title|default:m.site.hostname }}]{% endblock %}

{% block body %}
<p>{_ Hello _} {{ m.rsc[id].name_first|default:m.rsc[id].title }},</p>

<p>{_ This email address was added to your personal information on _} {{ m.site.title|default:m.site.hostname }}: {{ idn.key }}.</p>

{% with m.identity[id].username as username %}
{% if username %}
<p>{_ Your account name is _} “<strong>{{ username|escape }}</strong>”.</p>
{% endif %}
{% endwith %}

<p>{_ Click on the link to confirm your email address. Copy the whole link and paste it in your browser if this doesn't work. _}</p>

<p><a href="{% url identity_verify idn_id=idn.id verify_key=verify_key absolute_url %}">{% url identity_verify idn_id=idn.id verify_key=verify_key absolute_url %}</a></p>

<p>{_ If you don't know this site, please ignore this email. Maybe someone made an error. _}</p>
{% endblock %}

{% block disclaimer %}
<p style="color: #666; font-size: 80%;">--<br/>
{_ You received this email because you verified your email address. You will not receive any additional emails because of this request. _}</p>
{% endblock %}
