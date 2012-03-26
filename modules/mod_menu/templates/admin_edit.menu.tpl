{% extends "admin_edit.tpl" %}

{% block admin_edit_form_pre %}
{# Edit the menu #}
<div class="row edit-header">

    <div id="menu-editor" class="span6">
        {% include "_admin_menu_menu_view.tpl" id=id %}
    </div>

    <div class="span2">

        {% include "_menu_trash.tpl" %}
        
        <ul id="sortable-new" class="tree-list ui-sortable" title="{_ Drag to add a new page _}">
            <li id="page-new-text" data-nestedsortable-receive="new">
                <div class="clearfix">
                    <img class="grippy" src="/lib/images/grippy.png" alt="{_ Drag me _}" />
                    <span>{_ New page _}</span>
                </div>
            </li>
            {% draggable id="page-new-text" to_sorter=["menu-",id|make_list] helper='clone' %}
        </ul>
    </div>

    <div class="span4">
        <div class="widget">
            <h3 class="widget-header">{_ How does this work? _}</h3>
            <div class="widget-content">

        <p id="drag-explanation">
                {_ Type your search terms to find pages. Then drag them on to the panel on your left. _}
                {_ Drag items from the menu to the trash to remove them. _} 
                {_ Drag the <strong>New page</strong> to add a new page. _}
            </p>
            
            <div class="control-group">
                <input id="{{#input}}" class="autocompleter" type="text" value="" />
                <ul id="{{#suggestions}}" class="tree-list ui-sortable"></ul>
            </div>
            
            {% wire id=#input
            type="keyup"
            action={typeselect target=#suggestions template="_admin_menu_typeselect_result.tpl" id=id}
            %}
            </div>
        </div>
    </div>    
</div>
<hr/>
{% endblock %}
