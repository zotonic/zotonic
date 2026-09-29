{% if m.rsc[id].id as id %}
    {% if id.is_authoritative %}
    {% with `x-default` as z_language %}
        <link rel="hub" href="{% url websub absolute_url %}">
        <link rel="self" href="{% url websub_topic id=id absolute_url %}">
    {% endwith %}
    {% endif %}
{% endif %}
