{% extends "email_base.tpl" %}

{% block title %}{{ name|escape }} - {_ contact form on _} {{ m.config.site.title.value|default:m.site.hostname }}{% endblock %}

{% block body %}
<p>{_ Hello, the contact form of _} “{{m.config.site.title.value|default:m.site.hostname}}” {_ has been submitted. _}</p>

<table>
<tr>
    <td><strong>{_ Name _}</strong></td><td>{{ name|escape }}</td>
</tr>
<tr>
    <td><strong>{_ E-mail _}</strong></td><td>{{ email_from|escape }}</td>
</tr>
<tr>
    <td><strong>{_ Message _}</strong></td>
    <td><pre>{{ message|force_escape|linebreaksbr }}</pre></td>
</tr>
{% for name,value in fields %}
    {% if name /= "name" and name /= "mail" and name /= "message" %}
    <tr>
        <td><strong>{{ name|escape }}</strong></td><td>{{ value|escape }}</td>
    </tr>
    {% endif %}
{% endfor %}
</table>

{% endblock %}
