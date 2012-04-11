{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Sub pages _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-haspart{% endblock %}

{% block widget_content %}
{% if is_editable or m.rsc[id].haspart %}
<div id="{{ #haspart }}">
    <ul id="category" class="tree-list categories {% if editable %}do_menuedit{% endif %}" data-menuedit="connectWith: '#haspart-trash'">
        {% for mid in m.rsc[id].haspart %}
            {% if mid %}
            <li id="{{ #cat.mid }}">
                {% include "_menu_edit_item.tpl" id=mid %}
            </li>
            {% endif %}
        {% endfor %}
    </ul>
</div>
{% if is_editable %}
<div class="widget-footer">
    <div class="pull-right">
        {% include "_menu_trash.tpl" element_id="haspart-trash"%}

        delete button &amp; find page button
    </div>
</div>
{% endif %}

{% endif %}
{% endblock %}
