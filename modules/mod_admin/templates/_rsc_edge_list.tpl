{#
Params:
- id
- predicate
- unlink_action
#}
{% for o_id, edge_id in m.edge.o[id][predicate] %}
    {% include "_rsc_edge.tpl" subject_id=id predicate=predicate object_id=o_id 
        edge_id=edge_id unlink_action=unlink_action undo_message_id=undo_message_id %}
{% endfor %}
