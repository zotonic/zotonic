<tr class="{% if result_row.severity <= 1 %}log-error{% endif %}{% if result_row.severity == 2 %}log-warning{% endif %}" >
    <td>
	<a href="{% url admin_log_email severity=result_row.severity %}">
	    {% if result_row.severity == 0 %}Fatal{% endif %}
	    {% if result_row.severity == 1 %}Error{% endif %}
	    {% if result_row.severity == 2 %}Warning{% endif %}
	    {% if result_row.severity == 3 %}Info{% endif %}
	    {% if result_row.severity == 4 %}Debug{% endif %}
	</a>
    </td>
    <td title="{{ result_row.mailer_message }} [{{ result_row.mailer_host }}]">
	<a href="{% url admin_log_email severity=q.severity status=result_row.mailer_status %}">
	    {{ result_row.mailer_status|escape }}
	</a>
    </td>
    <td title="{{ result_row.message_nr }}">
	<a href="{% url admin_log_email severity=4 message_nr=result_row.message_nr %}">
	    {{ result_row.message_nr|truncate:12|escape }}
	</a>
    </td>
    <td title="{{result_row.envelop_to|escape}}">
	{% if id.to_id %}
	<a href="{% url admin_log_email severity=4 to=result_row.to_id %}">{{ result_row.to_id }}</a> / 
    	<a href="{% url admin_log_email severity=4 to=result_row.envelop_to %}">
    	    {{ result_row.envelop_to|truncate:10|escape }}
    	</a>
	{% else %}
	<a href="{% url admin_log_email severity=4 to=result_row.envelop_to %}">
	    {{ result_row.envelop_to|truncate:20|escape|default:"-" }}
	</a>
	{% endif %}
    </td>
    <td title="{{result_row.envelop_from|escape}}">
	{% if result_row.from_id %}
	<a href="{% url admin_log_email severity=4 from=result_row.from_id %}">{{ result_row.from_id }}</a> /
	<a href="{% url admin_log_email severity=4 from=result_row.envelop_from %}">
	    {{ result_row.envelop_from|truncate:10|escape }}
	</a>
	{% else %}
	<a href="{% url admin_log_email severity=4 from=result_row.envelop_from %}">
	    {{ result_row.envelop_from|truncate:20|escape|default:"-" }}
	</a>
	{% endif %}
    </td>
    <td>
	<a href="{% url admin_log_email severity=4 content=result_row.content_id %}">
	    {{ result_row.content_id|default:"-" }}
	</a>
    </td>
    <td>
	<a href="{% url admin_log_email severity=4 other=result_row.other_id %}">
	    {{ result_row.other_id|default:"-" }}
	</a>
    </td>
    <td>
	<a href="{% url admin_log_email severity=4 template=result_row.message_template %}">
	    {{ result_row.message_template|default:"-" }}
	</a>
    </td>
    <td>
        {{ result_row.created|date:"Y-m-d H:i:s" }}
    </td>
</tr>
