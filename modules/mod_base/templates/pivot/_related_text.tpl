{% for name,os in m.edge[id] %}
    {% if name /= 'author' and not m.rsc[name].is_object_noindex %}
        {% for o in os %}
            {% catinclude "pivot/_title.tpl" o.object_id %}
        {% endfor %}
    {% endif %}
{% endfor %}
{% if id.content_group_id %}
    {% catinclude "pivot/_title.tpl" id.content_group_id %}
{% endif %}
