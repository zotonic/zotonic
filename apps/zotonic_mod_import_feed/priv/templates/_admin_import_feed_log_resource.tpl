{% if id %}
    <a href="{% url admin_edit_rsc id=id %}">{{ id.title|default:id }} ({{ id }})</a>
{% else %}
    —
{% endif %}
