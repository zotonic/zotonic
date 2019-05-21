{% with signal_props.log_id|default:id as id %}
    {% with m.log_ui[id] as l %}
        {% if     (not qmessage or l.message|lower|match:(qmessage|lower))
              and (not quser or quser == l.user_id)
        %}
            {% if l.type == 'error' %}
                <tr class="text-danger">
            {% elseif l.type == 'debug' %}
                <tr class="text-muted">
            {% elseif l.type == 'info' %}
                <tr class="text-color">
            {% else %}
                <tr class="text-{{ l.type|default:"info" }}">
            {% endif %}
                <td>{{ l.created|date:"Y-m-d H:i:s" }}</td>
                <td>{{ l.type }}</td>
                <td>
                    {% if l.message|length > 200 %}
                        {{ l.message|truncate:255|force_escape|linebreaksbr }}
                        </pre>
                    {% else %}
                        {{ l.message|force_escape|linebreaksbr }}
                    {% endif %}
                    <span class="text-muted">
                        <br><small><span class="label label-default">URL</span> {{ l.url|escape }}</small>
                        <br><small><span class="label label-default">UA</span> {{ l.user_agent|escape }}</small>
                        <br><small><span class="label label-default">IP</span> {{ l.remote_ip|escape }}</small>
                    </span>
                </td>
                <td>
                    {% if l.user_id == 1 %}
                        <span class="text-muted">{_ Site Administrator _}</span>
                    {% elseif l.user_id %}
                        <a href="{% url admin_edit_rsc id=l.user_id %}">
                            {% if m.rsc[l.user_id].exists %}
                                {% include "_name.tpl" id=l.user_id %}
                                ({{ l.user_id }} / {{ l.user_id.email|default:"-" }})
                            {% else %}
                                {{ l.user_name_first }} {{ l.user_name_surname }}
                                ({{ l.user_id }} / {{ l.user_email_raw|escape|default:"-" }})
                            {% endif %}
                        </a>
                    {% endif %}
                </td>
                <td>
                    <tt>{{ l.stack|log_format_stack }}</tt>
                </td>
            </tr>
        {% endif %}
    {% endwith %}
{% endwith %}
