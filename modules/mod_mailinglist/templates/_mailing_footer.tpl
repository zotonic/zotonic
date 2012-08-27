{% if not list_id %}
<p style="color: #666">{_ You received this mail because someone wanted to send you this information. We did not store your e-mail address and will not send you any other mail because of this mail. _}</p>
{% else %}
<p style="color: #666">{_ You received this mail because you are subscribed to the mailing list _} <a href="http://{{ m.site.hostname }}{{ list_id.page_url }}">{{ list_id.title }}</a>.
{% if recipient_id %}
{#	{{ recipient_id }}
	Link to the person page of the recipient
	{% with m.identity[recipient_id].username as username %}
		{% if username %}
			- sign in for editing
		{% else %}
			- use unique id in identity table for mailing list access 
				[later: signup/claim by "all include"]
		{% endif %}
	{% endwith %}
#}
{% else %}
	{% with m.mailinglist.subscription[list_id][email] as sub %}
		<a href="http://{{ m.site.hostname }}{% url mailinglist_unsubscribe confirm_key=sub.confirm_key %}">{_ Please unsubscribe _}</a> {_ when you don't want to receive any further mail from this list. _}
	{% endwith %}
{% endif %}
</p>
{% endif %}
