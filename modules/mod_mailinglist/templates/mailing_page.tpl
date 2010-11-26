{% extends "email_base.tpl" %}

{% block title %}{{ m.rsc[id].title }}{% endblock %}

{% block body %}
<p style="text-align: center"><a href="{{ m.rsc[id].page_url }}">{_ Click here when you can’t read the message below. _}</a></p>

<hr/>

<h1>{{ m.rsc[id].title }}</h1>

{% if m.rsc[id].body %}
	<p><strong>{{ m.rsc[id].summary|linebreaksbr }}</strong></p>
	{{ m.rsc[id].body }}
{% else %}
	<p>{{ m.rsc[id].summary|linebreaksbr }}</p>
{% endif %}

<p><a href="{{ m.rsc[id].page_url }}">{_ Read this page on the web. _}</a></p>

<hr/>

{% with m.mailinglist.subscription[list_id][email] as sub %}
<p style="color: #666">{_ You received this mail because you are subscribed to the mailing list _} <a href="{{ m.rsc[list_id].page_url }}">{{ m.rsc[list_id].title }}</a>. <a href="{% url mailinglist_unsubscribe confirm_key=sub.confirm_key %}">{_ Please unsubscribe _}</a> {_ when you don't want to receive any further mail from this list. _} </p>
{% endwith %}

{% endblock %}
