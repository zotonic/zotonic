
{% with m.mailinglist.rsc_stats[id] as rsc_stats %}
{% with m.mailinglist.tasks[id] as mailing_tasks %}

<h3>{_ Mailing lists _}</h3>

{% if rsc_stats %}
<p>{{ m.rsc[id].title }} {_ has been sent to the following lists: _}</p>
{% else %}
<p>{{ m.rsc[id].title }} {_ has never been sent yet. _}</p>
{% endif %}

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
        </td>
        <td>
            {% if m.rsc[mid].name == "mailinglist_test" %}
                {% button class="btn btn-default btn-xs"
                          text=_"send test now"
                          action={mailing_page_test id=id}
                          title=_"Send immediately; the page does not need to be published."
                %}
            {% elif tasks %}
                {% button class="btn btn-default btn-xs"
                          text=_"cancel"
                          postback={dialog_mailing_cancel_confirm list_id=mid page_id=id}
                          delegate="mod_mailinglist"
                %}
            {% else %}
                {% if stats.total > rsc_stats[mid].total|default:0 %}
                    {% button class="btn btn-default btn-xs"
                              text=_"send mailing"
                              action={dialog_mailing_page id=id list_id=mid}
                              title=_"send to "|append:m.rsc[mid].title
                    %}
                {% else %}
                    {% button class="btn btn-default btn-xs"
                              text=_"clear"
                              action={confirm
                                text=_"Are you sure you want to reset the statistics for this mailing? This means that if you send the mailing again afterwards, recipients might have gotten the mailing twice."
                                postback={mailinglist_reset list_id=mid page_id=id}
                                delegate="mod_mailinglist"
                              }
                    %}
                {% endif %}
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
