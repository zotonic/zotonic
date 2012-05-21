{% extends "email_base.tpl" %}

{# Subject of the e-mail #}
{% block title %}{{ id.title }}{% endblock %}

{# Main body of the message sent. #}
{% block content %}
{% with m.mailinglist.subscription[list_id][email] as sub %}
<tr>
	<td id="content">
		{% image id.depiction width=200 height=300 align="left" style="float: left; margin: 0 10px 10px 0; padding: 0; border: 0;" %}
		<h1>{{ id.title }}</h1>
		{% if id.summary %}
			<p><b>{{ id.summary }}</b></p>
		{% endif %}
		{{ id.body|show_media:"_body_media_mailing.tpl"|inject_recipientdetails:sub }}
		{% include "_blocks.tpl" %}
	</td>
</tr>
{% endwith %}
{% endblock %}


{# Shown above the mail body. A link to read the message on the web. #}
{% block header %}
{% if id.is_published and not id.publication_start|in_future %}
<tr>
	<td>
		<p style="text-align: center;"><a href="http://{{ m.site.hostname }}{{ id.page_url }}" style="color: gray;">{_ Click here when you can’t read the message below. _}</a></p>
	</td>
</tr>
<tr>
	<td>&nbsp;</td>
</tr>
{% endif %}
{% endblock %}


{# Shown below the mail body. Reference to the mailinglist (if any) and unsubscribe links. #}
{% block footer %}
{% if not list_id.mailinglist_private %}
<tr>
	<td>&nbsp;</td>
</tr>
<tr>
	<td style="border-top: 1px solid #ccc;">{% include "_mailing_footer.tpl" %}</td>
</tr>
{% endif %}
{% endblock %}
