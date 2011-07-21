{% extends "admin_edit.tpl" %}

{% block admin_edit_form_pre %}
{# Edit the menu #}

<div class="zp-50">
    <div class="padding">
        <div id="menu-editor">
            {% include "_admin_menu_menu_view.tpl" id=id %}
        </div>
    </div>
</div>

<div class="item-wrapper search-nav-items zp-50">
    <h3 class="above-item">{_ Search for a page _}</h3>
    
    <div class="item">
        <div class="notification notice">
            {_ Here you can change the menu of your site.  Select pages on the right hand side and drag them to the menu on the left. _}
            <a href="javascript:void(0)" class="do_dialog" data-dialog="title: '{_ Help about editing menus. _}', text: '{{ _"Type the name of the page you want in your menu in the box left of this note. When you get a result, you can drag that item onto the menu pane on the left side of the page. You can also move the menu items around to manage the way your navigations looks."|escapejs }}', width: '450px'">{_ Need more help? _}</a>
        </div>
        
        <p>{_ Type your search terms to find pages. Then drag them on to the panel on your left. _}</p>
        
        <div class="form-item autocomplete-wrapper clear">
            <input id="{{#input}}" class="autocompleter" type="text" value="" />
            <ul id="{{#suggestions}}" class="short-list"></ul>
        </div>
        
        {% wire id=#input
        type="keyup"
        action={typeselect target=#suggestions template="_admin_menu_typeselect_result.tpl" id=id}
        %}
    </div>
</div>



{% endblock %}
