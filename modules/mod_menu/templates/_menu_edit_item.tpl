{% with menu_id|default:#menu.id as menu_id %}
<div id="{{ menu_id }}">
    <img class="grippy" src="/lib/images/grippy.png" title="{_ Drag me _}" />
    <span>{{ id.title }}</span>
    {% if id.is_editable %}
    <span class="pull-right">
        <a href="#" id="{{ #edit.id }}" class="btn btn-mini">{_ Edit _}</a>
        {% wire id=#edit.id action={dialog_edit_basics id=id target=menu_id template="_menu_edit_item.tpl"} %}
    </span>
    {% endif %}
</div>
{% endwith %}
