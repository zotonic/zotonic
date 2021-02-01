{#
Params:
- id
- predicate
- unlink_action
- is_list_truncated -- set for long lists without sorter
- edge_list_max_length -- max length of the edge list
#}
{% with (edge_list_max_length|default:100 / 2)|round as listlen %}
{% with edges|default:m.edge.o[id][predicate] as edges %}
    {% with edges|length as count %}
        {% if not is_list_truncated %}
            {% for o_id, edge_id in edges %}
                {% include "_rsc_edge.tpl" subject_id=id predicate=predicate object_id=o_id 
                    edge_id=edge_id unlink_action=unlink_action undo_message_id=undo_message_id %}
            {% endfor %}
        {% else %}
            {# Too many edges for usable drag/drop -- show first and last listlen #}
            {% for o_id, edge_id in edges %}
                {% if forloop.counter <= listlen or forloop.counter > count - listlen %}
                    {% include "_rsc_edge.tpl" subject_id=id predicate=predicate object_id=o_id
                        edge_id=edge_id unlink_action=unlink_action undo_message_id=undo_message_id
                        is_list_truncated %}
                {% elseif forloop.counter == listlen + 1  %}
                    <li class="alert alert-info menu-item-alert">
                        <span class="glyphicon glyphicon-alert"></span> {_ Too many connections, only the first and last are shown. _}
                        <a class="btn btn-default btn-xs" href="{% url admin_edges qhassubject=id qpredicate=predicate %}">{_ View all connections _}</a>
                    </li>
                {% endif %}
            {% endfor %}
        {% endif %}
    {% endwith %}
{% endwith %}
{% endwith %}

