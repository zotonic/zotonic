
{% with m.mailinglist.rsc_stats[id] as rsc_stats %}
{% with m.mailinglist.tasks[id] as mailing_tasks %}
{% with m.rsc[q.list_id].id as qlist_id %}

<h3>{_ Mailing lists _}</h3>

<p>
    {% if rsc_stats %}
        {% trans "The page “{title}” has been sent to the following lists:" title=m.rsc[id].title %}
    {% else %}
        {% trans "The page “{title}” has never been sent yet." title=m.rsc[id].title %}
    {% endif %}
</p>

<table class="table table-striped admin-table">
    <thead>
        <tr>
	    <th width="30%">{_ Title _}</th>
	    <th width="10%">{_ Actions _}</th>
	    <th width="10%">{_ Recipients _}</th>
	    <th width="15%">{_ Sent on / Status _}</th>
	    <th width="35%">{_ Statistics _}</th>
        </tr>
    </thead>

    <tbody>
    {% for title, mid in m.search[{all_bytitle cat="mailinglist" pagelen=1000}] %}
    {% with m.mailinglist.stats[mid] as stats %}
	{% with mailing_tasks[mid] as tasks %}
	<tr id="mailinglist-{{ mid }}">
	    <td>
            <a href="{% url admin_mailinglist_recipients id=mid %}">{{ title|default:"untitled" }}</a>

            {% if mid == qlist_id and m.rsc[mid].name != "mailinglist_test" %}
                {% if tasks %}
                    <br><small class="text-warning"><span class="glyphicon glyphicon-info-sign"></span> {_ Cancel the current mailing before sending again. _}</small>
                {% elseif stats.total =< rsc_stats[mid].total|default:0 %}
                    <br><small class="text-warning"><span class="glyphicon glyphicon-info-sign"></span> {_ Reset the statistics before sending. _}</small>
                {% endif %}
            {% endif %}
        </td>
        <td>
            {% if m.rsc[mid].name == "mailinglist_test" %}
                {% button class="btn btn-default btn-xs"
                          text=_"Send test now"
                          action={mailing_page_test id=id}
                          title=_"Send immediately; the page does not need to be published."
                %}
                {% if mid == qlist_id %}
                    {% wire action={confirm
                            title=_"Send to "|append:m.rsc[mid].title
                            action={mailing_page_test id=id}
                            text=_"Are you sure you want to send the page to test mailing list?"
                            ok=_"Send now"
                        }
                    %}
                {% endif %}
            {% elseif tasks %}
                {% button class="btn btn-default btn-xs"
                          text=_"Cancel"
                          postback={dialog_mailing_cancel_confirm list_id=mid page_id=id}
                          delegate="mod_mailinglist"
                %}
            {% elseif stats.total > rsc_stats[mid].total|default:0 %}
                {% button class="btn btn-default btn-xs"
                          text=_"Send mailing"
                          action={dialog_mailing_page id=id list_id=mid}
                          title=_"Send to "|append:m.rsc[mid].title
                %}
                {% if mid == qlist_id %}
                    {% wire action={dialog_mailing_page id=id list_id=mid} %}
                {% endif %}
            {% else %}
                {% button class="btn btn-default btn-xs"
                          text=_"Clear"
                          action={confirm
                            text=_"Are you sure you want to reset the statistics for this mailing? This means that if you send the mailing again afterwards, recipients might have gotten the mailing twice."
                            postback={mailinglist_reset list_id=mid page_id=id}
                            delegate="mod_mailinglist"
                          }
                %}
            {% endif %}
        </td>

        <td>{{ stats.total|format_number }}</td>

        <td>
            {% if rsc_stats[mid].created %}
                <a href="{% url admin_log_email content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].created|date:"Y-m-d H:i" }}</a>
            {% endif %}
            {% for task in tasks %}
                <div>
                    {% if task.type == "date" %}
                        <span class="label label-info">{_ scheduled _}</span>
                        {{ task.due|date:_"Y-m-d H:i" }}
                    {% else %}
                        <span class="label label-info">{_ scheduled _}</span>
                        {{ task.due|date:_"Y-m-d H:i" }}
                        <div class="text-muted">{_ when the page becomes published _}</div>
                    {% endif %}
                    <div class="text-muted">
                        {_ Sender: _} {% include "_name.tpl" id=task.sender_id %}
                    </div>
                    <div class="text-muted">
                        {_ Language: _} {% include "_mailinglist_task_language.tpl" language=task.language %}
                    </div>
                </div>
            {% empty %}
                {% if not rsc_stats[mid].created %}-{% endif %}
            {% endfor %}
        </td>

        <td>
            {% if rsc_stats[mid].created %}
                <a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].total|default:0 }} {_ processed _}</a>
                (<a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].sent|default:0 }} {_ OK _}</a>,
                <a href="{% url admin_log_email status='bounce' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].bounce|default:0 }} {_ bounced _}</a>,
                <a href="{% url admin_log_email status='error' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].error|default:0 }} {_ error _}</a>)
            {% else %}
                -
            {% endif %}
        </td>
    </tr>
	{% endwith %}
    {% endwith %}
	{% empty %}
        <tr>
            <td colspan="5">{_ No items found _}</td>
        </tr>
	{% endfor %}
    </tbody>
</table>

{% endwith %}
{% endwith %}
{% endwith %}
