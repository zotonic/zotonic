{% with signal_props.log_id|default:id as id %}
{% with m.log[id] as l %}
<li id="{{ #li.id }}" class="clearfix">
        <span class="zp-5">{{ l.type|default:"-" }}</span>
        <span class="zp-25">{{ l.module|default:"-" }}{% if l.line %}:{{ l.line }}{% endif %}</span>
        <span class="zp-45">
            {% if l.message|length > 200 %}
            <pre>
                {{ l.message|truncate:255|force_escape|linebreaksbr }}
            </pre>
            {% else %}
                {{ l.message|force_escape|linebreaksbr }}
            {% endif %}
        </span>
        <span class="zp-15">
            {% if l.user_id %}
            <a href="{% url admin_edit_rsc id=l.user_id %}">{{ m.rsc[l.user_id].title }}</a>
            {% else %}
            -
            {% endif %}
        </span>
        <span class="zp-10">{{ l.created|date:"d M Y, H:i" }}</span>
</li>
{% endwith %}
{% endwith %}
