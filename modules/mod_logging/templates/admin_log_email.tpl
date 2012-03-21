{% extends "admin_log_base.tpl" %}

{% block title %}{_ Log email _}{% endblock %}

{% block title_log %}{_ Log incoming/outgoing e-mail _}{% endblock %}

{% block active2 %}active{% endblock %}

{% block content_log %}

{#
-record(log_email, {
    severity = ?LOG_ERROR,
    message_nr,
    mailer_status,      % sending, sent, error, retry, warning, bounce, received
    mailer_message,     % any text, to clarify the mailer_status
    mailer_host,        % SMTP server or client we are talking with
    envelop_to,         % the 'to' on the envelop
    envelop_from,       % the 'from' on the envelop
    to_id,              % who is receiving the e-mail
    from_id,            % who is sending (user in the #context)
    content_id,         % The page being sent (if any)
    other_id,           % In case of a mailinglist the mailinglist id
    message_template,   % template used for rendering the e-mail (if any)
    props = []          % optional extra properties to be logged
}).
#}

		<h3 class="above-list ">
			{_ Most recent messages _}
		</h3>

		{% with m.search[{log_email
		                        page=q.page
		                        pagelen=20
		                        severity=q.severity
		                        status=q.status
		                        message_nr=q.message_nr
		                        to=q.to
		                        from=q.from
		                        content=q.content
		                        other=q.other
		                        template=q.template
		    }] as result 
		%}
		<form id="log_filter" method="GET" action="{% url log_email %}">
			<ul class="short-list">
				<li class="headers clearfix" id="log-headers">
					<span class="zp-10">{_ Severity _}</span>
					<span class="zp-10">{_ Status _}</span>
					<span class="zp-10">{_ Message nr _}</span>
					<span class="zp-15">{_ To _}</span>
					<span class="zp-15">{_ From _}</span>
					<span class="zp-5">{_ Content _}</span>
					<span class="zp-5">{_ Other _}</span>
					<span class="zp-15">{_ Template _}</span>
					<span class="zp-15">{_ Date _}</span>
				</li>
				<li class="headers clearfix" id="log-headers">
					<span class="zp-10">
					   <select id="log_severity" name="severity" style="width: 95%">
					       <option value="0" {% if q.severity == '0' %}selected="selected"{% endif %}>{_ Fatal _}</option>
					       <option value="1" {% if q.severity == '' or q.severity|is_undefined or q.severity == '1' %}selected="selected"{% endif %}>{_ Error _}</option>
					       <option value="2" {% if q.severity == '2' %}selected="selected"{% endif %}>{_ Warning _}</option>
					       <option value="3" {% if q.severity == '3' %}selected="selected"{% endif %}>{_ Info _}</option>
					       <option value="4" {% if q.severity == '4' %}selected="selected"{% endif %}>{_ Debug _}</option>
					   </select>
					   {% wire id="log_severity" type="change" action={submit target="log_filter"} %}
					</span>
					<span class="zp-10">
 					   <select id="log_status" name="status" style="width: 95%">
 					       <option value="">{_ All _}</option>
 					       <option value="sending" {% if q.status == 'sending' %}selected="selected"{% endif %}>{_ Sending _}</option>
 					       <option value="sent" {% if q.status == 'sent' %}selected="selected"{% endif %}>{_ Sent _}</option>
 					       <option value="bounce" {% if q.status == 'bounce' %}selected="selected"{% endif %}>{_ Bounce _}</option>
 					       <option value="received" {% if q.status == 'received' %}selected="selected"{% endif %}>{_ Received _}</option>
 					       <option value="failed" {% if q.status == 'failed' %}selected="selected"{% endif %}>{_ Failed _}</option>
 					       <option value="retry" {% if q.status == 'retry' %}selected="selected"{% endif %}>{_ Retry _}</option>
 					   </select>
					   {% wire id="log_status" type="change" action={submit target="log_filter"} %}
					</span>
					<span class="zp-10">
					   <input name="message_nr" type="text" style="width: 85%" value="{{ q.message_nr|escape }}" />
					</span>
					<span class="zp-15">
					   <input name="to" type="text" style="width: 85%" value="{{ q.to|escape }}" />
					</span>
					<span class="zp-15">
 					   <input name="from" type="text" style="width: 85%" value="{{ q.from|escape }}" />
					</span>
					<span class="zp-5">
					   <input name="content" type="text" style="width: 70%" value="{{ q.content|escape }}" />
					</span>
					<span class="zp-5">
 					   <input name="other" type="text" style="width: 70%" value="{{ q.other|escape }}" />
					</span>
					<span class="zp-15">
					   <input name="template" type="text" style="width: 85%" value="{{ q.template|escape }}" />
					</span>
					<span class="zp-15">
					   <button type="submit">{_ Filter _}</button>
					   <button id="filter_clear">{_ All _}</button>
					   {% wire id="filter_clear" 
					            action={set_value selector="#log_filter input" value=""}
					            action={set_value selector="#log_status" value=""}
					            action={set_value selector="#log_severity" value="4"}
					            action={submit target="log_filter"}
					    %}
					</span>
				</li>
			</ul>

		   <div class="wrapper" style="height: 620px; overflow: auto">
			   <ul class="short-list" id="log-area">
				{% for result_row in result %}
					{% include "_admin_log_email_row.tpl" %}
				{% empty %}
					<li>
						{_ No log messages. _}
					</li>
				{% endfor %}
				</ul>
			   {% button text="more..." 
			        action={moreresults result=result target="log-area" template="_admin_log_email_row.tpl"}
			    %}
		   </div>
		{% endwith %}

{% endblock %}
