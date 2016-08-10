{# Used on the resource edit page and by the medium upload event. Show all connected media. #}
{% with rsc_id|default:id as id %}
<div id="{{ #unlink_message }}"></div>
{% live template="_edit_media_list.tpl"
        id=id unlink_message=#unlink_message
        topic={object id=id predicate=`depiction`} %}
{% endwith %}
