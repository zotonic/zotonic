{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your subscription on _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
	{% include "_email_mailinglist_hello.tpl" %}

	<p>{_ You, or someone else, added your e-mail address to our mailing list _} <a href="{{ m.rsc[list_id].page_url_abs }}">{{ m.rsc[list_id].title }}</a>.
	{_ Before you will receive any further mail you need to confirm your subscription. _}</p>

	<p>{_ Please follow _} <a href="{% url mailinglist_confirm confirm_key=recipient.confirm_key absolute_url %}">{_ this link to confirm _}</a>, {_ or copy and paste the address below in your browser. _}</p>

	<p><a href="{% url mailinglist_confirm confirm_key=recipient.confirm_key absolute_url %}">{% url mailinglist_confirm confirm_key=recipient.confirm_key absolute_url %}</a></p>

	<p>{_ When you don’t want to receive any mail then please ignore this message. _}</p>
{% endblock %}
