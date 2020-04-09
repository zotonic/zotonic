{#
Params:
- id
- predicate
- unlink_action
- is_list_truncated -- set for long lists without sorter
#}
{% with edges|default:m.edge.o[id][predicate] as edges %}
    {% with edges|length as count %}
        {% if not is_list_truncated %}
            {% for o_id, edge_id in edges %}
                {% include "_rsc_edge.tpl" subject_id=id predicate=predicate object_id=o_id 
                    edge_id=edge_id unlink_action=unlink_action undo_message_id=undo_message_id %}
            {% endfor %}
        {% else %}
            {# Too many edges for usable drag/drop -- show first and last 30 #}
            {% for o_id, edge_id in edges %}
                {% if forloop.counter <= 30 or forloop.counter >= count - 30 %}
                    {% include "_rsc_edge.tpl" subject_id=id predicate=predicate object_id=o_id
                        edge_id=edge_id unlink_action=unlink_action undo_message_id=undo_message_id
                        is_list_truncated %}
                {% elseif forloop.counter == 31 %}
                    <li class="alert alert-info menu-item-alert">
                        <span class="glyphicon glyphicon-alert"></span> {_ Too many connections, only the first and last 30 are shown. _}
                        <a class="btn btn-default btn-xs" href="{% url admin_edges qhassubject=id qpredicate=predicate %}">{_ View all connections _}</>
                    </li>
                {% endif %}
            {% endfor %}
        {% endif %}
    {% endwith %}
{% endwith %}
