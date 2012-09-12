{# Show an object with an unlink option. Used in the admin_edit #}
{% with m.rsc[object_id].title as title %}
{% sortable id=#unlink_wrapper tag=edge_id %}
<li id="{{ #unlink_wrapper }}">
    <span class="btn btn-small" id="{{ #edit }}">
        <i title="{_ Drag to change connection position _}" class="unlink-mover icon-move"></i>
        <id id="{{ #unlink }}" title="{_ Disconnect _}" class="icon-remove"></id>
        <a href="{% url admin_edit_rsc id=object_id %}" title="{_ Edit _} {{ title }}">{{ title|truncate:30 }}</a>
    </span>
</li>
{% endwith %}

{% wire id=#unlink action={unlink subject_id=subject_id edge_id=edge_id hide=#unlink_wrapper} %}
{% wire id=#edit target=#unlink_wrapper action={dialog_edit_basics edge_id=edge_id} %}
