{% for b in id.blocks %}
    {% for z_language in id.language|default:[z_language] %}
        {{ b.header }} {{ b.body }}
    {% endfor %}
{% endfor %}
