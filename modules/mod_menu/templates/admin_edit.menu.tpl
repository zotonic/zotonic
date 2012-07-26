{% extends "admin_edit.tpl" %}

{% block admin_edit_form_pre %}
{# Edit the menu #}
<div class="row edit-header">

    <div id="menu-editor" class="span8">
        {% include "_admin_menu_menu_view.tpl" id=id %}
    </div>

    <div class="span4">
        <div class="widget">
            <h3 class="widget-header">{_ How does this work? _}</h3>
            <div class="widget-content">
            <p>
                {_ Click on <strong>Add menu item</strong> or <strong>Menu item</strong> to add pages. _}
                {_ Drag menu items in the menu up, down, left or right to structure the menu. _}
            </p>
            </div>
        </div>
    </div>    
</div>
{% endblock %}
