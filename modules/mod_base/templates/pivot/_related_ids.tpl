{# All related resource ids, used for finding similar resources #}
{# Prefix object ids with 'zpo' and categories with 'zpc' #}
{% for name,os in m.edge[id] %}
    {% for o in os %}
        zpo{{ o.object_id }}
    {% endfor %}
{% endfor %}
{% if id.content_group_id %}
    zpo{{ id.content_group_id }}
{% endif %}
{% for cat,_true in id.is_a %}
    zpc{{ m.rsc[cat].id }}
{% endfor %}
