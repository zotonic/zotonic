{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing Lists _}{% endblock %}

{% block content %}
<div class="admin-header">
    <h2>{_ Mailing lists _}</h2>

    <p>{_ Any page can be sent as a mailing. You can send a mailing from any edit page. On this page you can add or remove mailing lists and their recipients. _}<br/>
    {_ Recipients are subscribed either as email-only (via a simple signup form), or as subscribed persons in the system. _}</p>
</div>

<div class="well z-button-row">
    {% button class="btn btn-primary" text=_"New mailing list" action={dialog_new_rsc cat="mailinglist"} %}
</div>

<table class="table table-striped do_adminLinkedTable">
    <thead>
        <tr>
            <th width="20%">{_ Title _}</th>
            <th width="40%">{_ Description _}</th>
            <th width="10%">{_ Recipients _}</th>
            <th width="10%">{_ Scheduled _}</th>
	    </tr>
    </thead>

    <tbody>
	    {% for title, id in m.search[{all_bytitle cat="mailinglist" pagelen=1000}] %}
    	    <tr id="mailinglist-{{id}}" data-href="{% url admin_mailinglist_recipients id=id %}">
        		{% with m.rsc[id].is_editable as editable %}
        		    <td width="20%">{{ title|default:"untitled" }}</td>
        		    <td width="40%">{{ m.rsc[id].summary|default:"-" }}</td>
        		    {% with m.mailinglist.stats[id] as stats %}
        		    <td width="10%">{{ stats.total|format_number }}</td>
        		    <td width="30%">
        		        <div class="pull-right buttons">
        		            <a class="btn btn-default btn-xs" href="{% url admin_mailinglist_recipients id=id %}">{_ Recipients _}</a>
        			        {% if editable %}
                                <a class="btn btn-default btn-xs" href="{% url admin_edit_rsc id=id %}">{_ Edit _}</a>
            			    {% else %}
                                <a class="btn btn-default btn-xs" href="{% url admin_edit_rsc id=id %}">{_ View _}</a>
        			        {% endif %}
        			        {% button class="btn btn-default btn-xs" text=_"Delete" postback={mailinglist_delete_confirm id=id} disabled=not editable %}
        		        </div>
                        {{ stats.scheduled|length|format_number }}
                        </td>
        		    {% endwith %}
        		{% endwith %}
    	    </tr>
	    {% empty %}
    	    <tr>
                <td colspan="4"> {_ No items found _} </td>
            </tr>
	    {% endfor %}
    </tbody>
</table>

{% with m.mailinglist.tasks as mailing_tasks %}
<h3>{_ Pending mailings _}</h3>

<table class="table table-striped">
    <thead>
        <tr>
            <th>{_ Mailing _}</th>
            <th>{_ Mailing list _}</th>
            <th>{_ Scheduled for _}</th>
            <th>{_ Actions _}</th>
        </tr>
    </thead>
    <tbody>
        {% for task in mailing_tasks %}
            <tr>
                <td>
                    <a href="{% url admin_mailing_status id=task.page_id %}">
                        {{ m.rsc[task.page_id].title|default:_"untitled" }}
                    </a>
                </td>
                <td>
                    <a href="{% url admin_mailinglist_recipients id=task.mailinglist_id %}">
                        {{ m.rsc[task.mailinglist_id].title|default:_"untitled" }}
                    </a>
                </td>
                <td>
                    {% if task.type == "date" %}
                        <span class="label label-info">{_ delayed _}</span>
                        {{ task.due|date:_"Y-m-d H:i" }}
                    {% else %}
                        <span class="label label-info">{_ scheduled _}</span>
                        {{ task.due|date:_"Y-m-d H:i" }}
                        <div class="text-muted">{_ when the page becomes published _}</div>
                    {% endif %}
                </td>
                <td>
                    {% with m.rsc[task.mailinglist_id].is_editable as is_editable %}
                    {% button class="btn btn-default btn-xs"
                              text=_"cancel"
                              postback={dialog_mailing_cancel_confirm list_id=task.mailinglist_id page_id=task.page_id}
                              delegate="mod_mailinglist"
                              disabled=not is_editable
                    %}
                    {% endwith %}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="4">{_ No pending mailings. _}</td>
            </tr>
        {% endfor %}
    </tbody>
</table>
{% endwith %}
{% endblock %}
