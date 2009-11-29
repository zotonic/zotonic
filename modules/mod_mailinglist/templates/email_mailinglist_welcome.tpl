{% extends "email_base.tpl" %}

{% block title %}Welcome to {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
	<p>Hello,</p>

	<p>You are now subscribed to our mailing list <a href="{{ m.rsc[list_id].page_url }}">{{ m.rsc[list_id].title }}</a> with your e-mail address {{ recipient.email|escape }}. From now on you will receive mail from our mailing list.</p>

	<p>When you don’t want to receive any more mail then <a href="{% url mailinglist_unsubscribe confirm_key=recipient.confirm_key %}">click here to unsubscribe.</a></p>

	<p>Kind Regards,</p>

	<p><a href="/">{{ m.site.hostname }}</a></p>
{% endblock %}
