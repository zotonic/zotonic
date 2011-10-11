
{% with m.mailinglist.rsc_stats[id] as rsc_stats %}
{% with m.mailinglist.scheduled[id] as scheduled %}

{% if rsc_stats %}
<p>{{ m.rsc[id].title }} {_ has been sent to the following lists: _}</p>
{% else %}
<p>{{ m.rsc[id].title }} {_ has never been sent yet. _}</p>
{% endif %}
<h3 class="above-list">{_ Mailing lists _}</h3>
		<ul class="short-list">
			<li class="headers clearfix">
				<span class="zp-45">{_ Title _}</span>
				<span class="zp-10">{_ Recipients _}</span>
				<span class="zp-10">{_ Sent on _}</span>
				<span class="zp-25">{_ Statistics _}</span>
				<span class="zp-10">{_ Actions _}</span>
			</li>
		{% for title, mid in m.search[{all_bytitle cat="mailinglist"}] %}
            {% with m.mailinglist.stats[mid] as stats %}
			<li id="mailinglist-{{ mid }}" class="clearfix">

					<span class="zp-45">
                        <a href="{% url admin_mailinglist_recipients id=mid %}">{{ title|default:"untitled" }}</a>
                    </span>

                    <span class="zp-10">{{ stats[1]|format_number }}</span>

                    {% if rsc_stats[mid].created %}
                    <span class="zp-10">
                        <a href="{% url admin_log_email content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].created|date:"Y-m-d H:i" }}</a>
                        {% if mid|member:scheduled %}{_ scheduled _}{% endif %}
                    </span>
					<span class="zp-25">
                        {% if stats[1] > rsc_stats[mid].total %}
                        <a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].total|default:0 }} {_ processed _}</a>
                        {% else %}
                        <a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{_ All processed _}</a>
                        {% endif %}
                        (<a href="{% url admin_log_email status='sent' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].sent|default:0 }} {_ OK _}</a>,
                         <a href="{% url admin_log_email status='bounce' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].bounce|default:0 }} {_ bounced _}</a>,
                         <a href="{% url admin_log_email status='error' content=id other=mid severity=4 %}" title="{_ Click to view log entries _}">{{ rsc_stats[mid].error|default:0 }} {_ error _}</a>)
                    </span>

                {% else %}
                    <span class="zp-10">
                        {% if mid|member:scheduled %}{_ scheduled _}{% endif %}
                    </span>
					<span class="zp-25">
                        -
                    </span>
                {% endif %}

					<span class="button-area">
                        {% if rsc_stats[mid].bounce %}
                                {% button text=_"Bounces" title=_"View and edit the bounced addresses and re-send the mailing." 
                                        action={dialog_open template="_dialog_mailing_bounces.tpl" title=_"Bounces" id=id mid=mid} %}
                        {% endif %}
                        {% if mid|member:scheduled %}
                                {% button text=_"cancel" postback={dialog_mailing_cancel_confirm list_id=mid page_id=id} delegate="mod_mailinglist"  %}
                        {% else %}
                                {% if stats[1] > rsc_stats[mid].total|default:0 %}
                                {% button text=_"send mailing" action={dialog_mailing_page id=id list_id=mid} title=_"send to "|append:m.rsc[mid].title %}
                                {% else %}
                                {% button text=_"clear" action={confirm text=_"Are you sure you want to reset the statistics for this mailing? This means that if you send the mailing again afterwards, recipients might have gotten the mailing twice." postback={mailinglist_reset list_id=mid page_id=id} delegate="mod_mailinglist"} %}
                                {% endif %}
                        {% endif %}
					</span>
			</li>
            {% endwith %}
		{% empty %}
			<li>
				{_ No items found _}
			</li>
		{% endfor %}
		</ul>

{% endwith %}
{% endwith %}
