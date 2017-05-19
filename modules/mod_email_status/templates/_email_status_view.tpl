{% with email|default:(id.email_raw) as email %}
{% with m.email_status[email] as status %}
	<h3>
		{_ Information about _}
		&lt;{{ email|escape }}&gt;
	</h3>

	{% if status or email|is_valid_email %}
		{% if not status or status.is_valid %}
			{% if status.error_ct or status.bounce_ct %}
				<p class="alert alert-info">
					<span class="icon-envelope"></span>
					<strong>{_ This email address is now ok. _}</strong>
					{_ There have been some problems but they have been cleared. This message will disappear as soon as an email has been received succesfully. _}
				</p>
			{% else %}
				<p class="alert alert-success">
					<span class="icon-ok"></span>
					<strong>{_ This email address is ok. _}</strong>
				</p>
			{% endif %}
		{% else %}
			<p class="alert {% if status.error_is_final %}alert-error{% else %}alert-warning{% endif %}" style="margin-bottom: 20px">
				<span class="icon-envelope"></span>
				<strong>
					{_ There are problems with this email address. _}
					{% if not status.error_is_final or status.recent_error_ct < 5  %}
						{_ We are retrying email delivery. _}
					{% else %}
						{_ We stopped email delivery. _}
					{% endif %}
				</strong>
			</p>

			{% if (id and id.is_editable) or m.acl.use.mod_email_status %}
				<p>
					<a href="#" class="btn btn-default btn-danger" id="{{ #doreset }}">
						{_ Clear error flag for this email address. _}
					</a>
				</p>
				{% wire id=#doreset
						postback={email_status_reset email=email id=id
								  on_success={hide target=#doreset}
								  on_success={show target=#didreset}}
						delegate=`mod_email_status`
				%}
				<p class="alert alert-success" id="{{ #didreset }}" style="display:none">
					{_ The email address has been cleared, new emails will be sent. _}
				</p>
			{% endif %}
		{% endif %}
	{% else %}
		<p class="alert alert-warning">
			<span class="glyphicon glyphicon-envelope"></span>
			<strong>{_ This email address is not valid. _}</strong>
		</p>
	{% endif %}

	<table class="table table-striped">
		<tr>
			<th></th>
			<th>{_ Total/Status _}</th>
			<th>{_ Most Recent _}</th>
		</tr>
		<tr>
			<th>{_ Received from this address _}</th>
			<td>{{ status.receive_ct }}</td>
			<td>{{ status.receive|date:"Y-m-d H:i" }}</td>
		</tr>
		<tr>
			<th>{_ Sent to this address _}</th>
			<td>{{ status.sent_ct }}</td>
			<td>{{ status.sent|date:"Y-m-d H:i" }}</td>
		</tr>
		<tr>
			<th>{_ Errors since last clear _}</th>
			<td>{{ status.recent_error_ct }}</td>
			<td>{{ status.error|date:"Y-m-d H:i" }}</td>
		</tr>
		<tr>
			<th>{_ Total send errors _}</th>
			<td>{{ status.error_ct }}</td>
			<td></td>
		</tr>
		<tr>
			<th>{_ Last error status _}</th>
			<td colspan="2">{{ status.error_status|force_escape|linebreaksbr }}</td>
		</tr>
		<tr>
			<th>{_ Bounces _}</th>
			<td>{{ status.bounce_ct }}</td>
			<td>{{ status.bounce|date:"Y-m-d H:i" }}</td>
		</tr>
	</table>
{% endwith %}
{% endwith %}
