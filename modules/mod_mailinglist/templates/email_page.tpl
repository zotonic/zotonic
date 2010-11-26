{% extends "email_base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block body %}
<p>{_ This mail was sent to you because someone visiting _} {{ m.site.hostname }} {_ thought you would be interested. _}</p>
<p>{_ You can _} <a href="{{ m.rsc[id].page_url }}">{_ read the full article online. _}</a></p>

<hr/>

<h1>{{m.rsc[id].title}}</h1>

<p><strong>{{m.rsc[id].summary}}</strong></p>

{{ m.rsc[id].body }}

{% endblock %}
