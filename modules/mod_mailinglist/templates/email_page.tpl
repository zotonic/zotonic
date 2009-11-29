{% extends "email_base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block body %}
<p>This mail was send to you because someone visiting {{ m.site.hostname }} thought you would be interested.</p>
<p>You can <a href="{{ m.rsc[id].page_url }}">read the full article online.</a></p>

<hr/>

<h1>{{m.rsc[id].title}}</h1>

<p><strong>{{m.rsc[id].summary}}</strong></p>

{{ m.rsc[id].body }}

{% endblock %}
