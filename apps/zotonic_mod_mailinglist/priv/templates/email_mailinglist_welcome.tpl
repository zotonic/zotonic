{% extends "email_base.tpl" %}

{% block title %}{_ Welcome to _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
	{% include "_email_mailinglist_hello.tpl" %}
	<p>{_ You are now subscribed to our mailing list _} <a href="{{ m.rsc[list_id].page_url_abs }}">{{ m.rsc[list_id].title }}</a> {_ with your e-mail address _} {{ recipient.email|escape }}. {_ From now on you will receive mail from our mailing list. _}</p>

	<p>{_ When you don’t want to receive any more mail then _} <a href="{% url mailinglist_unsubscribe confirm_key=recipient.confirm_key absolute_url %}">{_ click here to unsubscribe. _}</a></p>
{% endblock %}
