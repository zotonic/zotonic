{% extends "admin_base.tpl" %}

{% block title %}{_ Mailing Lists _}{% endblock %}

{% block content %}
<div class="edit-header">
    <h2>{_ Mailing lists _}</h2>

    <p>{_ Any page can be sent as a mailing. You can send a mailing from any edit page. On this page you can add or remove mailing lists and their recipients. _}<br/>
    {_ Recipients are subscribed either as email-only (via a simple signup form), or as subscribed persons in the system. _}</p>
    
    <div class="well">
        {% button class="btn btn-primary" text=_"New mailing list" action={dialog_new_rsc cat="mailinglist"} %}
    </div>


    <h3>{_ Mailing list overview _}</h3>
    <hr class="clear" />
    
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
	    {% for title, id in m.search[{all_bytitle cat="mailinglist"}] %}
	    <tr id="mailinglist-{{id}}" data-href="{% url admin_mailinglist_recipients id=id %}">
		{% with m.rsc[id].is_editable as editable %}
		    <td width="20%">{{ title|default:"untitled" }}</td>
		    <td width="40%">{{ m.rsc[id].summary|default:"-" }}</td>
		    {% with m.mailinglist.stats[id] as stats %}
		    <td width="10%">{{ stats[1]|format_number }}</td>
		    <td width="30%">
		        <div class="pull-right">
		            <a class="btn btn-mini" href="{% url admin_mailinglist_recipients id=id %}">{_ Recipients _}</a>
			    {% if editable %}
                            <a class="btn btn-mini" href="{% url admin_edit_rsc id=id %}">{_ Edit _}</a>
			    {% else %}
                            <a class="btn btn-mini" href="{% url admin_edit_rsc id=id %}">{_ View _}</a>
			    {% endif %}
			    {% button class="btn btn-mini" text=_"Delete" postback={mailinglist_delete_confirm id=id} disabled=not editable %}
		        </div>
                        {{ stats[2]|length|format_number }}
                    </td>
		    {% endwith %}
		{% endwith %}
	    </tr>
	    {% empty %}
	    <tr>
                <td colspan="4">
		    {_ No items found _}
                </td>
            </tr>
	    {% endfor %}
        </tbody>
    </table>
</div>
{% endblock %}
