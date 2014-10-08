{% extends "admin_base.tpl" %}

{% block title %}{_ Category Hierarchy _}{% endblock %}

{% block content %}
{% with m.acl.is_admin as editable %}
<div class="edit-header">
    <h2>{_ Categories _}</h2>

    <p>{_ Categories are used to categorize all pages. Every page belongs to exactly one category.<br/>The categories are defined in a hierarchy. Here you can change that hierarchy. _}</p>
</div>
<div class="row">
    <div id="category-sorter" class="col-lg-8 col-md-8">
        {% include "_admin_category_sorter.tpl" %}
    </div>

    <div id="sidebar" class="col-lg-4 col-md-4">
        <div class="widget">
            <h3 class="widget-header">{_ How does this work? _}</h3>
            <div class="widget-content">
                <p>
                    {_ Use the <i class="glyphicon glyphicon-cog"></i> button to add or remove categories. _}
                </p>
                <p>
                    {_ Drag categories to the place where you want them in the hierarchy. _}
                </p>
            </div>
        </div>
    </div>
</div>
</div>
{% endwith %}
{% endblock %}
