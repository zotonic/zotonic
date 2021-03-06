{% extends "admin_base.tpl" %}

{% block title %}{_ Recipients for _} “{{ m.rsc[id].title }}”{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Recipients for _} “{{ m.rsc[id].title }}”</h2>
	{% if not m.rsc[id].is_editable %}
	<p>{_ You are not allowed to view or edit the recipients list. You need to have edit permission on the mailing list to change and view the recipients. _}</p>
	{% endif %}
</div>

{% if not m.rsc[id].is_editable %}
	<div class="well">
    {% button class="btn btn-default" text=_"cancel" action={redirect back} %}
</div>
{% else %}
<div>
	<p>{_ All recipients of the mailing list. You can upload or download this list, which must be a file with one e-mail address per line. _}<br/>
    	<a href="{% url admin_edit_rsc id=id %}">{_ Edit the mailing list &raquo; _}</a><br/>
	</p>

	<div class="well">
	    {% button class="btn btn-primary" text=_"Add recipient" title=_"Add a new recipient." postback={dialog_recipient_add id=id} %}
	    {% button class="btn btn-default" text=_"Download all" title=_"Download list of all active recipients." action={growl text=_"Downloading active recipients list. Check your download window."} action={redirect dispatch="mailinglist_export" id=id} %}
	    {% button class="btn btn-default" text=_"Upload file" title=_"Upload a list of recipients." action={dialog_open title=_"Upload a list of recipients."  template="_dialog_mailinglist_recipients_upload.tpl" id=id} %}
        {% button class="btn btn-default" text=_"Clear" action={confirm text=_"Delete all recipients from this list?" postback={recipients_clear id=id} delegate='controller_admin_mailinglist_recipients'} %}
        {% button class="btn btn-default" text=_"Combine…" action={dialog_open title=_"Combine mailing list" id=id template="_admin_dialog_mailinglist_combine.tpl"} %}

    </div>

    {% with m.mailinglist.stats[id] as stats %}
        <p>{_ Number of recipients on this list: _} <b>{{ stats.total|format_number }}</b></p>
        <table class="table admin-table">
            <tr>
                <th>{_ Recipients _}</th>
                <th>{_ Subscriber Edges _}</th>
                <th>{_ Matched via Query _}</th>
            </tr>
            <tr>
                <td>
                    {{ stats.recipients|format_number }} <span class="text-muted">{_ see below _}</span>
                </td>
                <td>
                    <a href="{% url admin_edges qhasobject=id qpredicate=`subscriberof` %}">
                        {{ stats.subscriberof|format_number }} {_ pages _}
                    </a>
                </td>
                <td>
                    <a href="{% url admin_overview_rsc qquery_id=id %}">
                        {{ stats.query_text|format_number }} {_ pages _}
                    </a>
                </td>
            </tr>
        </table>
    {% endwith %}
</div>

<div class="widget">
    <div class="row">
        {% with m.search.paged[{mailinglist_recipients id=id pagelen=150 page=q.page}] as recipients %}

        {% for list in recipients|vsplit_in:3 %}
            <div class="col-lg-4 col-md-4">
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
                                    {% button class="btn btn-default btn-xs" text=_"delete" title=_"Remove this recipient. No undo possible." postback={recipient_delete recipient_id=rcpt_id target=#target.rcpt_id} %}
                                </div>
                                {{ email|truncate:35|escape|default:"-" }}
                                {% include "_mailinglist_email_status_flag.tpl" email=email %}
                            </td>
                        </tr>
                        {% wire id=#enabled.rcpt_id postback={recipient_is_enabled_toggle recipient_id=rcpt_id} %}
                        {% wire id=#item.rcpt_id postback={dialog_recipient_edit id=id recipient_id=rcpt_id} %}
                    {% endfor %}
                    </tbody>
                </table>
            </div>
        {% endfor %}
    </div>
    {% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
    {% endwith %}


</div>

{% endif %}

{% endblock %}
