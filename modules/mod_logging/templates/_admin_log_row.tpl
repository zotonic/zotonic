{% with signal_props.log_id|default:id as id %}
{% with m.log[id] as l %}
<div class="alert alert-{{ l.type|default:"info" }}">
    <div class="pull-right">
        {% if l.user_id %}
        <a href="{% url admin_edit_rsc id=l.user_id %}">{{ m.rsc[l.user_id].title }}</a> @
        {% endif %}
        {{ l.created|date:"d M Y, H:i" }}
    </div>
    <h5>
        {% if l.type %}{{ l.type }}{% if l.module %} &mdash; {% endif %}{% endif %}
        {% if l.module %}{{ l.module|default:"-" }}{% if l.line %}:{{ l.line }}{% endif %}{% endif %}
    </h5>
    <div>
        {% if l.message|length > 200 %}
        <pre>
            {{ l.message|truncate:255|force_escape|linebreaksbr }}
        </pre>
        {% else %}
        {{ l.message|force_escape|linebreaksbr }}
        {% endif %}
    </div>
</div>
{% endwith %}
{% endwith %}
