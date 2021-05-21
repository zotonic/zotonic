{% extends "email_base.tpl" %}

{# Subject of the e-mail #}
{% block title %}{{ id.title }}{% if not list_id %} - {{ m.site.title }}{% endif %}{% endblock %}

{# Main body of the message sent. #}
{% block body %}
	{% with m.mailinglist.subscription[list_id][email] as sub %}
	{% filter inject_recipientdetails:sub %}
		{% inherit %}
	{% endfilter %}
	{% endwith %}
{% endblock %}


{# Shown below the mail body. Reference to the mailinglist (if any) and unsubscribe links. #}
{% block footer %}
	{% inherit %}
	{% if not list_id or not list_id.mailinglist_private %}
		{% include "_mailing_footer.tpl" %}</td>
	{% endif %}
{% endblock %}
