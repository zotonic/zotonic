{% with m.mailinglist.tasks as mailing_tasks %}
<h3>{_ Pending mailings _}</h3>

<table class="table table-striped">
    <thead>
        <tr>
            <th>{_ Mailing _}</th>
            <th>{_ Mailing list _}</th>
            <th>{_ Sender _}</th>
            <th>{_ Language _}</th>
            <th>{_ Scheduled for _}</th>
            <th>{_ Actions _}</th>
        </tr>
    </thead>
    <tbody>
        {% for task in mailing_tasks %}
            <tr>
                <td>
                    <a href="{% url admin_mailing_status id=task.page_id %}">
                        {{ m.rsc[task.page_id].title|default:_"Untitled" }}
                    </a>
                </td>
                <td>
                    <a href="{% url admin_mailinglist_recipients id=task.mailinglist_id %}">
                        {{ m.rsc[task.mailinglist_id].title|default:_"Untitled" }}
                    </a>
                </td>
                <td>{% include "_name.tpl" id=task.sender_id %}</td>
                <td>{% include "_mailinglist_task_language.tpl" language=task.language %}</td>
                <td>
                    {% if task.type == "date" %}
                        <span class="label label-info">{_ scheduled _}</span>
                        {{ task.due|date:_"Y-m-d H:i" }}
                    {% else %}
                        <span class="label label-info">{_ delayed _}</span>
                        {{ task.due|date:_"Y-m-d H:i" }}
                        <div class="text-muted">{_ when the page becomes published _}</div>
                    {% endif %}
                </td>
                <td>
                    {% with m.rsc[task.mailinglist_id].is_editable as is_editable %}
                    {% button class="btn btn-default btn-xs"
                              text=_"Cancel"
                              postback={dialog_mailing_cancel_confirm list_id=task.mailinglist_id page_id=task.page_id}
                              delegate="mod_mailinglist"
                              disabled=not is_editable
                    %}
                    {% endwith %}
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="6">{_ No pending mailings. _}</td>
            </tr>
        {% endfor %}
    </tbody>
</table>
{% endwith %}

<h3>{_ All mailing lists _}</h3>

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
            {% if id.is_visible %}
                <tr id="mailinglist-{{ id }}" data-href="{% url admin_mailinglist_recipients id=id %}">
                    {% with m.rsc[id].is_editable as editable %}
                    {% with m.mailinglist.stats[id] as stats %}
                        <td width="20%">
                            <a href="{% url admin_edit_rsc id=id %}">{{ title|default:"untitled" }}</a>
                            {% if id.name == 'mailinglist_test' %}
                                <br><span class="label label-default">{_ Test mailing list _}</span>
                            {% endif %}
                        </td>
                        <td width="40%">
                            {{ m.rsc[id].summary|default:"-" }}
                        </td>
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
            {% endif %}
        {% empty %}
            <tr>
                <td colspan="4">{_ No mailing lists found. _}</td>
            </tr>
        {% endfor %}
    </tbody>
</table>
