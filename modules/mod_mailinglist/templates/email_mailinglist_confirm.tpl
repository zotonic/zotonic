{% extends "email_base.tpl" %}

{% block title %}Please confirm your subscription on {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
	<p>Hello,</p>

	<p>You, or someone else, added your e-mail address to our mailing list <a href="{{ m.rsc[list_id].page_url }}">{{ m.rsc[list_id].title }}</a>.
	Before you will receive any further mail you need to confirm your subscription.</p>

	<p>Please follow <a href="{% url mailinglist_confirm confirm_key=recipient.confirm_key %}">this link to confirm</a>, or copy and paste the address below in your browser.</p>

	<p><a href="{% url mailinglist_confirm confirm_key=recipient.confirm_key %}">http://{{m.site.hostname}}{% url mailinglist_confirm confirm_key=recipient.confirm_key %}</a></p>

	<p>When you don’t want to receive any mail then please ignore this message.</p>

	<p>Kind Regards,</p>

	<p><a href="/">{{ m.site.hostname }}</a></p>
{% endblock %}
