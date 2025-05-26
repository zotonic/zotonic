{% extends "email_base.tpl" %}

{% block title %}{_ Please confirm your subscription on _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
	{% include "_email_mailinglist_hello.tpl" %}

	<p>
		{% trans "You, or someone else, added your e-mail address to our mailing list <b><a href=\"{url}\">{title}</a></b>."
			url=m.rsc[list_id].page_url_abs
			title=m.rsc[list_id].title
		%}
		{_ Before you will receive any further mail you need to confirm your subscription. _}
	</p>

	<p>
		{% trans "Please follow <a href=\"{url}\">this link to confirm</a>,  or copy and paste the address below in your browser."
				url={mailinglist_confirm confirm_key=recipient.confirm_key}|url_abs
		%}
	</p>

	<p><a href="{% url mailinglist_confirm confirm_key=recipient.confirm_key absolute_url %}">{% url mailinglist_confirm confirm_key=recipient.confirm_key absolute_url %}</a></p>

	<p>{_ If you don’t want to receive any mail then please ignore this message. _}</p>

    {{ m.rsc[list_id].subscription_info_html|show_media }}
{% endblock %}
