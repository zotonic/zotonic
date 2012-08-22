{% extends "mailing_page.tpl" %}

{# Main body of the message sent. #}
{% block content %}
{% with m.mailinglist.subscription[list_id][email] as sub %}
<tr>
	<td id="content">
		{% image id width=800 height=800 style="margin: 0 0 10px 0; padding: 0; border: 0;" %}
		<h1>{{ id.title }}</h1>
		{% if id.summary %}
			<p><b>{{ id.summary }}</b></p>
		{% endif %}
		{{ id.body|inject_recipientdetails:sub }}
		{% include "_blocks.tpl" %}
	</td>
</tr>
{% endwith %}
{% endblock %}
