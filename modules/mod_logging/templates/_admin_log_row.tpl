{% with signal_props.log_id|default:id as id %}
    {% with m.log[id] as l %}
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
            </td>

            <td>
                    {% if l.user_id %}
                        <a href="{% url admin_edit_rsc id=l.user_id %}">{{ m.rsc[l.user_id].title }} ({{ l.user_id }})</a>
                    {% endif %}
            </td>

            <td>
                    {% if l.module %}{{ l.module|default:"-" }}{% if l.line %}:{{ l.line }}{% endif %}{% endif %}
            </td>
        </tr>
    {% endwith %}
{% endwith %}
