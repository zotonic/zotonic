{% extends "email_base.tpl" %}

{% block title %}You are now unsubscribed from {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
<p>Goodbye,</p>

<p>You are now unsubscribed from the mailing list <a href="{{ m.rsc[list_id].page_url }}">{{ m.rsc[list_id].title }}</a>.
<br/>Hope to see you again.</p>

<p>Kind Regards,</p>

<p><a href="/">{{ m.site.hostname }}</a></p>

{% endblock %}