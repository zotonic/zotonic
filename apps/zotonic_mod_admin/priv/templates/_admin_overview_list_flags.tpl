{% if id.is_protected %}
    &nbsp; <span class="fa fa-lock text-muted" title="{_ Protected, not deletable _}"></span>
{% endif %}
{% if id == 1 or id.is_a.meta or id.content_group_id.name == 'system_content_group' %}
    <span class="label label-warning pull-right hidden-xs" title="{_ This is system content. _}">
        {{ id.name|default:_"system content" }}
    </span>
{% elseif id.name %}
    <span class="label label-default pull-right hidden-xs">
        {{ id.name }}
    </span>
{% endif %}
{% if m.identity[id].username as username %}
    <span class="label label-info pull-right hidden-xs">{{ username | escape }}</span>
{% endif %}
