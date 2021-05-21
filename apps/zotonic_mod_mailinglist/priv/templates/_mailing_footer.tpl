{% if not list_id %}
	<p style="font-family: sans-serif; color: #dddddd; text-align: center; padding: 20px 40px;">{_ You received this mail because someone wanted to send you this information. We did not store your e-mail address and will not send you any other mail because of this mail. _}</p>
{% else %}
	<p style="font-family: sans-serif; color: #dddddd; text-align: center; padding: 20px 80px 0 80px;">
		{_ You received this mail because you are subscribed to the mailing list _}
		<a href="{{ list_id.page_url_abs }}" style="color: #dddddd; text-decoration: underline;">{{ list_id.title }}</a>.

		{% if m.mailinglist.subscription[list_id][email] as sub %}
			<br><br>
			<a style="color: #dddddd; text-decoration: underline;" href="{% url mailinglist_unsubscribe confirm_key=sub.confirm_key absolute_url %}">{_ Please unsubscribe _}</a> {_ if you don't want to receive any further mail from this list. _}
		{% endif %}
	</p>
{% endif %}
