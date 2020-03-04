{% with email|default:(id.email_raw) as email %}
{% with m.email_status[email] as status %}
	{% if m.acl.is_admin %}
		{% if not status.is_blocked %}
			<button class="btn btn-danger btn-xs pull-right" id="{{ #doblock }}">
				{_ [ADMIN] _} {_ Block _}
			</button>
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
				{_ This email address has been blocked, no emails will be sent. _}
			</p>
		{% else %}
			<p>
				<button class="btn btn-success btn-xs pull-right" id="{{ #doreset }}">
					{_ [ADMIN] _} {_ Unblock _}
				</button>
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
					{_ The email address has been cleared, new emails will be sent. _}
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
				<p class="alert alert-warning">
					<span class="glyphicon glyphicon-envelope"></span>
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
			<p class="alert alert-error" style="margin-bottom: 20px">
				<span class="glyphicon glyphicon-envelope"></span>
				<strong>{_ There are problems with this email address. _}</strong>
			</p>

			{% if (id and id.is_editable) or m.acl.use.mod_email_status %}
				<p>
					<button class="btn btn-default btn-danger" id="{{ #doclear }}">
						{_ Clear error flag for this email address. _}
					</button>
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
				{% endif %}
				<p class="alert alert-success" id="{{ #didclear }}" style="display:none">
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
