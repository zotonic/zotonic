{% extends "email_base.tpl" %}

{% block title %}{{ name|escape }} - {_ contact form on _} {{ m.config.site.title.value|default:m.site.hostname }}{% endblock %}

{% block body %}
<p>{_ Hello, the contact form of _} “{{m.config.site.title.value|default:m.site.hostname}}” {_ has been submitted. _}</p>

<p>{_ Name _}: {{ name|escape }}</p>
<p>{_ E-mail _}: {{ mail|escape }}</p>

<p>{_ The contents of the message was: _}</p>
<pre>{{ message|force_escape|linebreaksbr }}</pre>
{% endblock %}
