{% extends "admin_base.tpl" %}

{% block title %}{_ Recipients for _} “{{ m.rsc[id].title }}”{% endblock %}

{% block content %}
<div class="edit-header">

    <h2>{_ Recipients for _} “{{ m.rsc[id].title }}”</h2>

	{% if not m.rsc[id].is_editable %}
	<p>{_ You are not allowed to view or edit the recipients list. You need to have edit permission on the mailing list to change and view the recipients. _}</p>
	<div class="well">
	    {% button class="btn" text=_"cancel" action={redirect back} %}
    </div>
	{% else %}

	<p>{_ All recipients of the mailing list. You can upload or download this list, which must be a file with one e-mail address per line. _}<br/>
	<a href="{% url admin_edit_rsc id=id %}">{_ Edit the mailing list &raquo; _}</a><br/>
        {% if id.s.subscriberof|length > 0 %}
	<a href="{% url admin_referrers id=id %}">{_ See the _}  {{ id.s.subscriberof|length }} {_ subscribing persons &raquo; _}</a>.
        {% endif %}
	</p>

	<div class="well">
	    {% button class="btn btn-primary" text=_"Add recipient" title=_"Add a new recipient." postback={dialog_recipient_add id=id} %}
	    {% button class="btn" text=_"Download all" title=_"Download list of all active recipients." action={growl text=_"Downloading active recipients list. Check your download window."} action={redirect dispatch="mailinglist_export" id=id} %}
	    {% button class="btn" text=_"Upload file" title=_"Upload a list of recipients." action={dialog_open title=_"Upload a list of recipients."  template="_dialog_mailinglist_recipients_upload.tpl" id=id} %}
            {% button class="btn" text=_"Clear" action={confirm text=_"Delete all recipients from this list?" postback={recipients_clear id=id} delegate='controller_admin_mailinglist_recipients'} %}

        </div>
</div>

<div class="row">
    {% with m.search.paged[{mailinglist_recipients id=id pagelen=150 page=q.page}] as recipients %}

    {% for list in recipients|vsplit_in:3 %}
    <div class="span4">
	<h3>{_ Recipients _}</h3>
        <table class="table table-striped">
            <thead>
                <tr>
		    <th width="10%">{_ Act _}</th>
		    <th width="90%">{_ Email _}</th>
                </tr>
            </thead>

            <tbody>
            {% for rcpt_id, email, is_enabled in list %}
                <tr class="{% if not is_enabled %}unpublished{% endif %}" id="{{ #target.rcpt_id }}">
                    <td width="10%"><input id="{{ #enabled.rcpt_id }}" title="{_ Check to activate the e-mail address. _}" type="checkbox" value="{{ rcpt_id }}" {% if is_enabled %}checked="checked"{% endif %} /></td>
                    <td width="90%" id="{{ #item.rcpt_id }}" style="cursor: pointer" title="{_ Edit recipient _}">
                        <div class="pull-right">
                            {% button class="btn btn-mini" text=_"delete" title=_"Remove this recipient. No undo possible." postback={recipient_delete recipient_id=rcpt_id target=#target.rcpt_id} %}
                        </div>
                        {{ email|truncate:35|escape|default:"-" }}
                    </td>
                </tr>
                {% wire id=#enabled.rcpt_id postback={recipient_is_enabled_toggle recipient_id=rcpt_id} %}
                {% wire id=#item.rcpt_id postback={dialog_recipient_edit id=id recipient_id=rcpt_id} %}
            {% endfor %}
            </tbody>
        </table>
    </div>
    {% empty %}
    <p>{_ No recipients _}</p>
    {% endfor %}
	
</div>
{% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
    {% endwith %}
    {% endif %}

{% endblock %}
