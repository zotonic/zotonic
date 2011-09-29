{% with menu_id|default:#menu.id as menu_id %}
<div id="{{ menu_id }}" class="clearfix menu-edit">
	<span class="grippy"><img src="/lib/images/grippy.png" alt="{_ Drag me _}" /></span>
	<span>{{ id.title }}</span>
	{% if id.is_editable %}
		<a href="#" id="{{ #edit.id }}" class="button">{_ Edit _}</a>
		{% wire id=#edit.id action={dialog_edit_basics id=id target=menu_id template="_menu_edit_item.tpl"} %}
	{% endif %}
</div>
{% endwith %}
