{% if keyword.id.is_editable %}
    <a href="{% url admin_edit_rsc id=keyword.id %}">{{ keyword.title|escape }}</a>
{% else %}
    <span>{{ keyword.title|escape }}</span>
{% endif %}
