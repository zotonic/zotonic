{# The email status can only be shown if either:
 # - The email is a verified email address of the current user.
 # - The email is the primary email address of the current user.
 # - The user has use.mod_email_status rights.
 # - The user is an admin.
 #}
{% with email|default:(id.email_raw) as email %}
{% if m.email_status[email] as status %}
	{% with m.acl.is_admin or m.acl.is_allowed.use.mod_email_status as is_allow_details %}

	{% if is_allow_details %}
		{% if not status.is_blocked %}
			<a href="#" class="btn btn-danger btn-small pull-right" id="{{ #doblock }}"
			   title="{_ Prevent sending email to, and receiving email from this address. _}">
				{_ Block _}
			</a>
			{% if panel_id %}
				{% wire id=#doblock
						postback={email_status_block email=email
								  on_success=[
								  	{update
								  		target=panel_id
								  		template="_email_status_view.tpl"
								  		email=email
								  		panel_id=panel_id
								  	}
								  ]}
						delegate=`mod_email_status`
				%}
			{% else %}
				{% wire id=#doblock
						postback={email_status_block email=email 
								  on_success=[
								  		{hide target=#doblock},
								  		{hide target=#isok},
								  		{show target=#didblock}
								  ]}
						delegate=`mod_email_status`
				%}
			{% endif %}

			<p class="alert alert-danger" id="{{ #didblock }}" style="display:none">
				<span class="glyphicon glyphicon-envelope"></span>
				<strong>
					{_ Blocked _}
				</strong>
				{_ This email address has been blocked, no emails will be sent or received from this address. _}
			</p>
		{% else %}
			<p>
				<a href="#" class="btn btn-success btn-small pull-right" id="{{ #doreset }}">
					{_ Unblock _}
				</a>
			</p>
			{% if panel_id %}
				{% wire id=#doreset
						postback={email_status_reset email=email id=id
								  on_success=[
								  	{update
								  		target=panel_id
								  		template="_email_status_view.tpl"
								  		email=email
								  		panel_id=panel_id
								  	}
								  ]}
						delegate=`mod_email_status`
				%}
			{% else %}
				{% wire id=#doreset
						postback={email_status_reset email=email id=id
								  on_success=[
								  		{hide target=#doreset},
								  		{hide target=#isblocked},
								  		{show target=#didreset}
								  ]}
						delegate=`mod_email_status`
				%}
				<p class="alert alert-success" id="{{ #didreset }}" style="display:none">
					{_ The block has been cleared and emails are being sent and received. _}
				</p>
			{% endif %}
		{% endif %}
	{% endif %}

	<h3>
		{_ Information about _}
		&lt;{{ email|escape }}&gt;
	</h3>

	{% if not email|is_valid_email %}
		<p class="alert alert-warning">
			<span class="glyphicon glyphicon-envelope"></span>
			<strong>{_ This email address is not valid. _}</strong>
		</p>
	{% elseif status.is_blocked %}
			<p class="alert alert-danger" style="margin: 20px 0" id="{{ #isblocked }}">
				<span class="glyphicon glyphicon-envelope"></span>
				<strong>
					{_ Blocked _}
				</strong>
				{_ This email address has been blocked. _}
				{_ We stopped email delivery. _}
			</p>
	{% elseif not status or status.is_valid %}
		{% if status.error_ct or status.bounce_ct %}
			<p class="alert alert-info" style="margin: 20px 0">
				<span class="glyphicon glyphicon-envelope"></span>
				<strong>{_ This email address is now ok. _}</strong>
				{_ There have been some problems but they have been cleared. This message will disappear as soon as an email has been received succesfully. _}
			</p>
		{% else %}
			<p class="alert alert-success" id="{{ #isok }}" style="margin: 20px 0">
				<span class="glyphicon glyphicon-ok"></span>
				<strong>{_ This email address is ok. _}</strong>
			</p>
		{% endif %}
	{% else %}
		<p class="alert {% if status.error_is_final %}alert-danger{% else %}alert-warning{% endif %}" style="margin: 20px 0">
			<span class="glyphicon glyphicon-envelope"></span>
			<strong>
				{_ There are problems with this email address. _}
				{% if not status.error_is_final or status.recent_error_ct < 5  %}
					{_ We are retrying email delivery. _} {_ If you are sure this email address is correct, the problem was probably temporary so you can clear the error message. _}
				{% else %}
					{_ We stopped email delivery. _}
				{% endif %}
			</strong>
		</p>

		{% if (id and id.is_editable) or m.acl.use.mod_email_status %}
			<p>
				<a href="#" class="btn btn-success" id="{{ #doclear }}">
					{_ Clear error for this email address. _}
				</a>
			</p>
			{% if panel_id %}
				{% wire id=#doclear
						postback={email_status_reset email=email id=id
								  on_success=[
								  	{update
								  		target=panel_id
								  		template="_email_status_view.tpl"
								  		email=email
								  		panel_id=panel_id
								  	}
								  ]}
						delegate=`mod_email_status`
				%}
			{% else %}
				{% wire id=#doclear
						postback={email_status_reset email=email id=id
								  on_success={hide target=#doclear}
								  on_success={show target=#didclear}}
						delegate=`mod_email_status`
				%}
				<p class="alert alert-success" id="{{ #didclear }}" style="display:none">
					{_ The error message has been cleared and emails are being sent. _}
				</p>
			{% endif %}
		{% endif %}
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

	{% if not hide_log %}
		{% if not status.log %}
			<p class="text-muted">{_ No recent email activity for this address. _}</p>
		{% else %}
			<details style="margin-bottom: 20px">
				<summary>{% trans "Recent email activity for &lt;{email}&gt;" email=email|escape %}</summary>
				<div>
					{% if is_allow_details %}
						<p class="help-block">
							<span class="glyphicon glyphicon-info-sign"></span>
							{_ The template name and mail server messages are hidden for regular users. _}
							{% if m.acl.is_allowed.use.mod_logging and m.modules.active.mod_logging %}
								<a href="{% url admin_log_email severity=4 to=email %}">
									{_ View email log _}
								</a>
							{% endif %}
						</p>
					{% endif %}
					<table class="table table-compact">
						<thead>
							<tr>
								<th>{_ Status _}</th>
								{% if is_allow_details %}
									<th>{_ Template _}</th>
								{% endif %}
								<th>{_ From _}</th>
								<th>{_ Date _}</th>
							</tr>
						</thead>
						<tbody>
							{#  0 -> fatal
				             #  1 -> error
				             #  2 -> warning
				             #  3 -> info
				             #  4 -> debug
							 #}
					        {% with ["text-danger", "text-danger", "text-warning", "text-info", ""] as level_class %}
					        {% with %{
								sending: _"Being sent",
								sent: _"Sent to recipient",
								relayed: _"Accepted for delivery",
								bounce: _"Returned by recipient",
								failed: _"Could not be sent",
								retry: _"Will try again",
								blocked: _"Sending blocked",
								received: _"Received from sender"
							} as mailer_status
					        %}
								{% for log in status.log %}
									<tr class="{{ level_class[log.severity + 1] }}">
										<td>{{ mailer_status[log.mailer_status]|default:log.mailer_status|escape }}</td>
										{% if is_allow_details %}
											<td>{{ log.message_template|escape }}</td>
										{% endif %}
										<td>{{ log.envelop_from|escape }}</td>
										<td>{{ log.created|date:"Y-m-d H:i" }}</td>
									</tr>
									{% if is_allow_details and log.mailer_message %}
										<tr>
											<td style="border-top: 0; padding-top: 0"></td>
											<td colspan="{% if is_allow_details %}3{% else %}2{% endif %}" style="border-top: 0; padding-top: 0">
												<small class="text-muted">{{ log.mailer_message|escape }}</small>
											</td>
										</tr>
									{% endif %}
								{% empty %}
									<tr>
										<td colspan="{% if is_allow_details %}4{% else %}3{% endif %}" class="text-muted">
											<span class="text-muted">{_ No recent email activity for this address. _}</span>
										</td>
									</tr>
								{% endfor %}
							{% endwith %}
							{% endwith %}
						</tbody>
					</table>
				</div>
			</details>
		{% endif %}
	{% endif %}

	{% endwith %}
{% else %}
	<p>
		{% trans "No email activity information is available for the email address &lt;{email}&gt;." email=email|escape %}
	</p>
{% endif %}
{% endwith %}
