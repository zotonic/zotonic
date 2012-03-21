{% extends "admin_base.tpl" %}

{% block title %}{_ Category Hierarchy _}{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
<div class="edit-header">

    <h2>{_ Categories _}</h2>

    <p>{_ Categories are used to categorize all pages. Every page belongs to exactly one category.<br/>The categories are defined in a hierarchy. Here you can change that hierarchy. _}</p>
    
</div>
<hr />
<div class="row">

    <div id="category-sorter" class="span6">
        {% include "_admin_category_sorter.tpl" %}
    </div>

        <div class="span2">
            {% include "_menu_trash.tpl" %}

            <ul id="sortable-new" class="tree-list ui-sortable" title="{_ Drag to add a new page _}">
                <li class="header" id="page-new-category">
                    <div class="clearfix">
                        <img class="grippy" src="/lib/images/grippy.png" alt="{_ Drag me _}" />
                        <span>{_ New category _}</span>
                    </div>
                </li>
                {% draggable id="page-new-category" to_sorter="category" helper='clone' %}
            </ul>
        </div>
    
    <div id="sidebar" class="span4">
        <div class="widget">
            <h3 class="widget-header">{_ How does this work? _}</h3>
            <div class="widget-content">
        <p>
            {_ Drag categories to the place where you want them in the hierarchy. _}
        </p>
        <p>
            {_ Drag categories to the trash to remove them. _} 
            {_ Drag the <strong>New category</strong> to add a new category. _}
        </p>
            </div>
        </div>

    </div>
</div>
</div>
{% endwith %}
{% endblock %}
