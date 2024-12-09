{% for b in id.blocks %}
    {% for z_language in id.language|default:[z_language] %}
        {{ b.body }} {{ b.header }} {{ b.prompt }}
    {% endfor %}
{% endfor %}
