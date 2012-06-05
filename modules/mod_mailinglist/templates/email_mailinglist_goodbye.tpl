{% extends "email_base.tpl" %}

{% block title %}{_ You are now unsubscribed from _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
{% include "_email_mailinglist_hello.tpl" %}

<p>{_ You are now unsubscribed from the mailing list _} <a href="http://{{ m.site.hostname }}{{ m.rsc[list_id].page_url }}">{{ m.rsc[list_id].title }}</a>. {_ Hope to see you again. _}</p>

<p>{_ Kind Regards, _}</p>

<p><a href="http://{{ m.site.hostname }}/">{{ m.config.site.title.value }}</a></p>

{% endblock %}
