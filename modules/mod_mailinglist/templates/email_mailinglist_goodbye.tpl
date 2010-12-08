{% extends "email_base.tpl" %}

{% block title %}{_ You are now unsubscribed from _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
<p>{_ Goodbye, _}</p>

<p>{_ You are now unsubscribed from the mailing list _} <a href="{{ m.rsc[list_id].page_url }}">{{ m.rsc[list_id].title }}</a>.
<br/>{_ Hope to see you again. _}</p>

<p>{_ Kind Regards, _}</p>

<p><a href="/">{{ m.site.hostname }}</a></p>

{% endblock %}
