{% if m.rsc[id].id as id %}
    {% with `x-default` as z_language %}
        <link rel="hub" href="{% url websub absolute_url %}">
        <link rel="self" href="{% url id id=id absolute_url %}">
    {% endwith %}
{% endif %}
