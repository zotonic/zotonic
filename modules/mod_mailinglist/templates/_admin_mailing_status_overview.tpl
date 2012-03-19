
{% with m.mailinglist.rsc_stats[id] as rsc_stats %}
{% with m.mailinglist.scheduled[id] as scheduled %}

{% if rsc_stats %}
<p>{{ m.rsc[id].title }} {_ has been sent to the following lists: _}</p>
{% else %}
<p>{{ m.rsc[id].title }} {_ has never been sent yet. _}</p>
{% endif %}
<h3>{_ Mailing lists _}</h3>
<hr/>

<table class="table">
    <thead>
        <tr>
	    <th width="40%">{_ Title _}</th>
	    <th width="10%">{_ Recipients _}</th>
	    <th width="15%">{_ Sent on _}</th>
	    <th width="35%">{_ Statistics _}</th>
        </tr>
    </thead>

    {% for title, mid in m.search[{all_bytitle cat="mailinglist"}] %}
    {% with m.mailinglist.stats[mid] as stats %}
    <tbody>
	<tr id="mailinglist-{{ mid }}">
	    <td>
                <a href="{% url admin_mailinglist_recipients id=mid %}">{{ title|default:"untitled" }}</a>
            </td>

            <td>{{ stats[1]|format_number }}</td>

            <td>
                {% if rsc_stats[mid].created %}
                <a href="{% url admin_log_email content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].created|date:"Y-m-d H:i" }}</a>
                {% if mid|member:scheduled %}{_ scheduled _}{% endif %}
                {% else %}
                {% if mid|member:scheduled %}{_ scheduled _}{% endif %}
                {% endif %}
            </td>

	    <td>
                <div class="pull-right">
                        {% if rsc_stats[mid].bounce %}
                                {% button class="btn btn-mini" text=_"Bounces" title=_"View and edit the bounced addresses and re-send the mailing." 
                                        action={dialog_open template="_dialog_mailing_bounces.tpl" title=_"Bounces" id=id mid=mid} %}
                        {% endif %}
                        {% if mid|member:scheduled %}
                                {% button class="btn btn-mini" text=_"cancel" postback={dialog_mailing_cancel_confirm list_id=mid page_id=id} delegate="mod_mailinglist"  %}
                        {% else %}
                                {% if stats[1] > rsc_stats[mid].total|default:0 %}
                                {% button class="btn btn-mini" text=_"send mailing" action={dialog_mailing_page id=id list_id=mid} title=_"send to "|append:m.rsc[mid].title %}
                                {% else %}
                                {% button class="btn btn-mini" text=_"clear" action={confirm text=_"Are you sure you want to reset the statistics for this mailing? This means that if you send the mailing again afterwards, recipients might have gotten the mailing twice." postback={mailinglist_reset list_id=mid page_id=id} delegate="mod_mailinglist"} %}
                                {% endif %}
                        {% endif %}
                </div>

                {% if rsc_stats[mid].created %}
                {% if stats[1] > rsc_stats[mid].total %}
                <a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].total|default:0 }} {_ processed _}</a>
                {% else %}
                <a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{_ All processed _}</a>
                {% endif %}
                (<a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].sent|default:0 }} {_ OK _}</a>,
                <a href="{% url admin_log_email status='bounce' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].bounce|default:0 }} {_ bounced _}</a>,
                <a href="{% url admin_log_email status='error' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].error|default:0 }} {_ error _}</a>)
                {% else %}
                -
                {% endif %}
            </td>
        </tr>
        {% endwith %}
	{% empty %}
        <tr>
            <td colspan="4">
		{_ No items found _}
            </td>
        </tr>
	{% endfor %}
    </tbody>
</table>

{% endwith %}
{% endwith %}
