{% with email|default:(id.email_raw) as email %}
{% with m.email_status[email] as status %}
	{% if m.acl.is_admin or m.acl.is_allowed.use.mod_email_status %}
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

	{% if status or email|is_valid_email %}
		{% if status.is_blocked %}
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
			<p class="alert {% if status.error_is_final %}alert-error{% else %}alert-warning{% endif %}" style="margin: 20px 0">
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
