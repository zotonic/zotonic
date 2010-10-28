{% extends "admin_base.tpl" %}

{% block title %}Recipients for “{{ m.rsc[id].title }}”{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>Recipients for “{{ m.rsc[id].title }}”</h2>

	{% if not m.rsc[id].is_editable %}
		<p>You are not allowed to view or edit the recipients list. You need to have edit permission on the mailing list to change and view the recipients.</p>
		
		{% button text="cancel" action={redirect back} %}
	{% else %}

		{% button text="Add recipient" title="Add a new recipient." postback={dialog_recipient_add id=id} %}
		{% button text="Download all" title="Download list of all active recipients." action={growl text="Downloading active recipients list. Check your download window."} action={redirect dispatch="mailinglist_export" id=id} %}
		{% button text="Upload file" title="Upload a list of recipients." action={dialog_open template="_dialog_mailinglist_recipients_upload.tpl" id=id} %}

		<hr class="clear" />
		<p>All recipients of the mailing list. You can upload or download this list, which must be a file with one e-mail address per line. <br/><a href="{% url admin_edit_rsc id=id %}">Edit the mailing list &raquo;</a></p>
		{% with m.search.paged[{mailinglist_recipients id=id pagelen=150 page=q.page}] as recipients %}
			{% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
			{% for list in recipients|vsplit_in:3 %}
				<div class="zp-30">
					<h3 class="above-list ">Recipients</h3>
					<ul class="short-list">
						<li class="headers clearfix">
							<span class="zp-10">Act</span>
							<span class="zp-70">Email</span>
							<span class="zp-20">Actions</span>
						</li>
					{% for rcpt_id, email, is_enabled in list %}
						<li id="recipient-{{rcpt_id}}">
							<a href="#" class="clearfix" id="{{ #item.rcpt_id }}">
								<span class="zp-10"><input id="{{ #enabled.rcpt_id }}" title="Check to activate the e-mail address." type="checkbox" value="{{ rcpt_id }}" {% if is_enabled %}checked="checked"{% endif %} /></span>
								<span class="zp-70">{{ email|escape|default:"-" }}</span>
								<span class="zp-20">
									{% button text="delete" title="Remove this recipient. No undo possible." postback={recipient_delete recipient_id=rcpt_id} %}
								</span>
							</a>
						</li>
						
						{% wire id=#enabled.rcpt_id postback={recipient_is_enabled_toggle recipient_id=rcpt_id} %}
						{% wire id=#item.rcpt_id postback={dialog_recipient_edit id=id recipient_id=rcpt_id} %}
					{% endfor %}
					</ul>
				</div>
				{% if not forloop.last %}
				<div class="zp-5">
					&nbsp;
				</div>
				{% endif %}
			{% empty %}
				<p>No recipients</p>
			{% endfor %}
			
			<div class="clear"></div>
			{% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
		{% endwith %}
	{% endif %}
	</div>
{% endblock %}
