<ul id="{{ #list }}" class="media list-unstyled">
    {% for object_id, edge_id in m.edge.o[id].depiction %}
        {% include "_rsc_edge_media.tpl" subject_id=id unlink_message=unlink_message %}
    {% endfor %}
</ul>
{% sorter id=#list
    tag={object_sorter predicate=`depiction` id=id}
    placeholder="ui-sortable-placeholder"
    delegate=`controller_admin_edit`
%}
