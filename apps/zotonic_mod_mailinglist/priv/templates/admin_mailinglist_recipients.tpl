{% extends "admin_base.tpl" %}

{% block title %}{_ Recipients for _} “{{ m.rsc[id].title }}”{% endblock %}

{% block content %}
<ul class="breadcrumb">
    <li><a href="{% url admin_mailinglist %}">{_ Mailing lists _}</a></li>
    <li class="active">{% trans "Recipients for “{title}”" title=m.rsc[id].title %}</li>
</ul>

<div class="admin-header">
    <h2>{_ Recipients for _} “{{ m.rsc[id].title }}”</h2>
	{% if not m.rsc[id].is_editable %}
	<p>{_ You are not allowed to view or edit the recipients list. You need to have edit permission on the mailing list to change and view the recipients. _}</p>
	{% endif %}
</div>

{% if id == m.rsc.mailinglist_test.id %}
    {% include "_mailinglist_test_public_alert.tpl" %}
{% endif %}

{% if not m.rsc[id].is_editable %}
	<div class="well">
    {% button class="btn btn-default" text=_"cancel" action={redirect back} %}
</div>
{% else %}
<div>
	<p>
        {_ All recipients of the mailing list. You can upload or download this list, which must be a file with one e-mail address per line. _}
	</p>

	<div class="well">
        <a class="btn btn-primary" href="{% url admin_edit_rsc id=id %}">{_ Edit list _}</a>
        {% button class="btn btn-primary" text=_"Add recipient" title=_"Add a new recipient." postback={dialog_recipient_add id=id} %}
	    {% button class="btn btn-default" text=_"Download all" title=_"Download list of all active recipients." action={growl text=_"Downloading active recipients list. Check your download window."} action={redirect dispatch="mailinglist_export" id=id} %}
	    {% button class="btn btn-default" text=_"Upload file" title=_"Upload a list of recipients." action={dialog_open title=_"Upload a list of recipients."  template="_dialog_mailinglist_recipients_upload.tpl" id=id} %}
        {% button class="btn btn-default" text=_"Clear" action={confirm text=_"Delete all recipients from this list?" postback={recipients_clear id=id} delegate='controller_admin_mailinglist_recipients'} %}
        {% button class="btn btn-default" text=_"Combine…" action={dialog_open title=_"Combine mailing list" id=id template="_admin_dialog_mailinglist_combine.tpl"} %}

    </div>

    <div id="mailinglist-recipient-counts">
        {% include "_admin_mailinglist_recipient_counts.tpl" id=id %}
    </div>
</div>

<form method="get" action="{% url admin_mailinglist_recipients id=id %}" class="form-inline well">
    <div class="form-group">
        <label for="recipient-sort">{_ Sort by _}</label>
        <select id="recipient-sort" name="qsort" class="form-control">
            <option value="email" {% if not q.qsort or q.qsort == "email" %}selected{% endif %}>{_ Email _}</option>
            <option value="newest" {% if q.qsort == "newest" %}selected{% endif %}>{_ Newest _}</option>
            <option value="oldest" {% if q.qsort == "oldest" %}selected{% endif %}>{_ Oldest _}</option>
        </select>
    </div>
    <div class="form-group">
        <label for="recipient-status">{_ Status _}</label>
        <select id="recipient-status" name="qstatus" class="form-control">
            <option value="all" {% if not q.qstatus or q.qstatus == "all" %}selected{% endif %}>{_ All _}</option>
            <option value="enabled" {% if q.qstatus == "enabled" %}selected{% endif %}>{_ Enabled _}</option>
            <option value="disabled" {% if q.qstatus == "disabled" %}selected{% endif %}>{_ Disabled _}</option>
        </select>
    </div>
    <div class="form-group">
        <label for="recipient-language">{_ Language _}</label>
        <select id="recipient-language" name="qlanguage" class="form-control">
            <option value="all" {% if not q.qlanguage or q.qlanguage == "all" %}selected{% endif %}>{_ All _}</option>
            <option value="none" {% if q.qlanguage == "none" %}selected{% endif %}>{_ Not set _}</option>
            {% for code, lang in m.translation.language_list_configured|language_sort_localized %}
                <option value="{{ code|escape }}" {% if q.qlanguage == code %}selected{% endif %}>{{ lang.name_localized|default:code|escape }}</option>
            {% endfor %}
        </select>
    </div>
    <button type="submit" class="btn btn-primary">{_ Filter _}</button>
</form>

{% with m.search.paged[{mailinglist_recipients id=id qsort=q.qsort qstatus=q.qstatus qlanguage=q.qlanguage pagelen=150 page=q.page}] as recipients %}
<div class="widget">
    <div class="row">
        {% for list in recipients|vsplit_in:3 %}
            <div class="col-lg-4 col-md-4">
                <table class="table table-striped">
                    <thead>
                        <tr>
                            <th title="{_ Enabled _}">✓</th>
                		    <th>{_ Email _}</th>
                        </tr>
                    </thead>

                    <tbody>
                    {% for rcpt_id, email, is_enabled, pref_language in list %}
                        <tr class="{% if not is_enabled %}unpublished{% endif %}" id="{{ #target.rcpt_id }}">
                            <td>
                                <input id="{{ #enabled.rcpt_id }}" title="{_ Check to activate the e-mail address. _}" type="checkbox" value="{{ rcpt_id }}" {% if is_enabled %}checked="checked"{% endif %}>
                            </td>
                            <td id="{{ #item.rcpt_id }}" style="cursor: pointer" title="{_ Edit recipient _} {{ email|escape }}">
                                <div class="pull-right">
                                    {% button class="btn btn-default btn-xs"
                                              text=_"delete"
                                              title=_"Remove this recipient. No undo possible."
                                              postback={recipient_delete
                                                    recipient_id=rcpt_id
                                                    target=#target.rcpt_id
                                              }
                                    %}
                                </div>
                                {{ email|truncatechars:35|escape|default:"-" }}
                                {% include "_mailinglist_email_status_flag.tpl" email=email %}
                                {% if pref_language %}
                                    <small class="text-muted">{{ m.translation.localized_name[pref_language]|default:pref_language|escape }}</small>
                                {% endif %}
                            </td>
                        </tr>
                        {% wire id=#enabled.rcpt_id
                                target=#target.rcpt_id
                                postback={recipient_is_enabled_toggle recipient_id=rcpt_id}
                        %}
                        {% wire id=#item.rcpt_id
                                postback={dialog_recipient_edit id=id recipient_id=rcpt_id}
                        %}
                    {% endfor %}
                    </tbody>
                </table>
            </div>
        {% endfor %}
    </div>
    {% pager result=recipients dispatch="admin_mailinglist_recipients" id=id qargs %}
</div>
{% endwith %}

{% endif %}

{% endblock %}
