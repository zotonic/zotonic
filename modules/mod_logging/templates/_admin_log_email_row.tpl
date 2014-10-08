<tr class="{% if result_row.severity <= 1 %}log-error{% endif %}{% if result_row.severity == 2 %}log-warning{% endif %}" >
    {#
        Severity
    #}
    <td>
        {% with ["Fatal", "Error", "Warning", "Info", "Debug"] as levels %}
            {% with levels[result_row.severity|format_integer + 1] as level %}
                <a href="{% url admin_log_email severity=result_row.severity %}" title="{{ level }}">{{ level }}</a>
            {% endwith %}
        {% endwith %}
    </td>
    {#
        Status
    #}
    <td>
        <a href="{% url admin_log_email severity=q.severity status=result_row.mailer_status %}" title="{{ result_row.mailer_message|make_list|escape }} [{{ result_row.mailer_host|escape }}]">
            {{ result_row.mailer_status|escape }}
        </a>
    </td>
    {#
        Message nr
    #}
    <td>
        <a href="{% url admin_log_email severity=4 message_nr=result_row.message_nr %}" title="{{ result_row.message_nr }}">
            {{ result_row.message_nr|truncate:12|escape }}
        </a>
    </td>
    {#
        To
    #}
    <td>
        {% if id.to_id %}
            <a href="{% url admin_log_email severity=4 to=result_row.to_id %}" title="{{result_row.to_id|escape}}">{{ result_row.to_id }}</a> / 
            <a href="{% url admin_log_email severity=4 to=result_row.envelop_to %}" title="{{result_row.envelop_to|escape}}">
                {{ result_row.envelop_to|truncate:10|escape }}
            </a>
        {% else %}
            <a href="{% url admin_log_email severity=4 to=result_row.envelop_to %}" title="{{result_row.envelop_to|escape}}">
                {{ result_row.envelop_to|truncate:20|escape|default:"-" }}
            </a>
        {% endif %}
    </td>
    {#
        From
    #}
    <td>
        {% if result_row.from_id %}
            <a href="{% url admin_log_email severity=4 from=result_row.from_id %}" title="{{result_row.from_id|escape}}">{{ result_row.from_id }}</a> /
            <a href="{% url admin_log_email severity=4 from=result_row.envelop_from %}" title="{{result_row.envelop_from|escape}}">
                {{ result_row.envelop_from|truncate:10|escape }}
            </a>
        {% else %}
            <a href="{% url admin_log_email severity=4 from=result_row.envelop_from %}" title="{{result_row.envelop_from|escape}}">
                {{ result_row.envelop_from|truncate:20|escape|default:"-" }}
            </a>
        {% endif %}
    </td>
    {#
        Content
    #}
    <td>
        <a href="{% url admin_log_email severity=4 content=result_row.content_id %}" title="{{ result_row.content_id|default:"-" }}">
            {{ result_row.content_id|default:"-" }}
        </a>
    </td>
    {#
        Other
    #}
    <td>
        <a href="{% url admin_log_email severity=4 other=result_row.other_id %}" title="{{ result_row.other_id|default:"-" }}">
            {{ result_row.other_id|default:"-" }}
        </a>
    </td>
    {#
        Template
    #}
    <td>
        <a href="{% url admin_log_email severity=4 template=result_row.message_template %}" title="{{ result_row.message_template|default:"-" }}">
            {{ result_row.message_template|default:"-" }}
        </a>
    </td>
    {#
        Date
    #}
    <td>
        {{ result_row.created|date:"Y-m-d H:i:s" }}
    </td>
</tr>
