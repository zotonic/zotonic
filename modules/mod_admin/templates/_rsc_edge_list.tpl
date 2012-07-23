{% for o_id, edge_id in m.edge.o[id][predicate] %}
    {% include "_rsc_edge.tpl" subject_id=id predicate=predicate object_id=o_id edge_id=edge_id %}
{% endfor %}
