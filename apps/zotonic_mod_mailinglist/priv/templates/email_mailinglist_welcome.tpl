{% extends "email_base.tpl" %}

{% block title %}{_ Welcome to _} {{ m.rsc[list_id].title }}{% endblock %}

{% block body %}
	{% include "_email_mailinglist_hello.tpl" %}
	<p>
		{% trans "You are now subscribed to our mailing list <b><a href=\"{url}\">{title}</a></b> with your e-mail address <b>{email}</b>."
				url=m.rsc[list_id].page_url_abs
				title=m.rsc[list_id].title
				email=recipient.email|escape
		%}
		{_ From now on you will receive mail from our mailing list. _}
	</p>
	<p>
		{% trans "If you don’t want to receive any more mail then <a href=\"{url}\">click here to unsubscribe</a>."
				url={mailinglist_unsubscribe confirm_key=recipient.confirm_key absolute_url}|url_abs
		%}
	</p>
    {{ m.rsc[list_id].subscription_info_html|show_media }}
{% endblock %}
