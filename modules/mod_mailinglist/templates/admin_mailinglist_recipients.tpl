{% extends "admin_base.tpl" %}

{% block title %}{_ Recipients for _} “{{ m.rsc[id].title }}”{% endblock %}

{% block content %}
	<div id="content" class="zp-85">
		<div class="block clearfix">

		<h2>{_ Recipients for _} “{{ m.rsc[id].title }}”</h2>

	{% if not m.rsc[id].is_editable %}
		<p>{_ You are not allowed to view or edit the recipients list. You need to have edit permission on the mailing list to change and view the recipients. _}</p>
		
		{% button text=_"cancel" action={redirect back} %}
	{% else %}

		{% button text=_"Add recipient" title=_"Add a new recipient." postback={dialog_recipient_add id=id} %}
		{% button text=_"Download all" title=_"Download list of all active recipients." action={growl text=_"Downloading active recipients list. Check your download window."} action={redirect dispatch="mailinglist_export" id=id} %}
		{% button text=_"Upload file" title=_"Upload a list of recipients." action={dialog_open template="_dialog_mailinglist_recipients_upload.tpl" id=id} %}
        {% button text=_"Clear" action={confirm text=_"Delete all recipients from this list?" postback={recipients_clear id=id} delegate='resource_admin_mailinglist_recipients'} %}

		<hr class="clear" />
		<p>{_ All recipients of the mailing list. You can upload or download this list, which must be a file with one e-mail address per line. _}<br/>
			<a href="{% url admin_edit_rsc id=id %}">{_ Edit the mailing list &raquo; _}</a><br/>
            {% if id.s.subscriberof|length > 0 %}
			<a href="{% url admin_referrers id=id %}">{_ See the _}  {{ id.s.subscriberof|length }} {_ subscribing persons &raquo; _}</a>.
            {% endif %}
		</p>
		{% with m.search.paged[{mailinglist_recipients id=id pagelen=150 page=q.page}] as recipients %}
			{% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
			{% for list in recipients|vsplit_in:3 %}
				<div class="zp-30">
					<h3 class="above-list ">{_ Recipients _}</h3>
					<ul class="short-list">
						<li class="headers clearfix">
							<span class="zp-10">{_ Act _}</span>
							<span class="zp-70">{_ Email _}</span>
							<span class="zp-20">{_ Actions _}</span>
						</li>
					{% for rcpt_id, email, is_enabled in list %}
						<li id="recipient-{{rcpt_id}}" class="{% if not is_enabled %}unpublished{% endif %}">
                            <span class="zp-10"><input id="{{ #enabled.rcpt_id }}" title=_"Check to activate the e-mail address." type="checkbox" value="{{ rcpt_id }}" {% if is_enabled %}checked="checked"{% endif %} /></span>
							<a href="#" class="clearfix" id="{{ #item.rcpt_id }}">
								<span class="zp-70">{{ email|escape|default:"-" }}</span>
								<span class="zp-20">
									{% button text=_"delete" title=_"Remove this recipient. No undo possible." postback={recipient_delete recipient_id=rcpt_id} %}
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
				<p>{_ No recipients _}</p>
			{% endfor %}
			
			<div class="clear"></div>
			{% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
		{% endwith %}
	{% endif %}
	</div>
{% endblock %}
