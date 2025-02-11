{# Used with the _action_dialog_connect.tpl to display a selected resource. #}

{% with select_id|default:id as id %}
    {% live template="_rsc_item.tpl" id=id catinclude topic=id %}
{% endwith %}
