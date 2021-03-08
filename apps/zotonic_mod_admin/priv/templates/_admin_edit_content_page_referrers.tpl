{% extends "admin_edit_widget_std.tpl" %}

{# Widget for showing the most recent incoming connections to a rsc #}

{% block widget_title %}
{_ Recent referrers _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Page referrers"|escapejs }}', text: '{{ _"This page can be connected from other pages. For example it could be an author of an article."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-referrers{% endblock %}

{% block widget_content %}
    <ul class="tree-list connections-list">
        {% for s_id in m.search[{query hasobject=id sort="-seq" pagelen=10}] %}
            <li id="{{ #unlink_wrapper }}" class="menu-item">
                <div class="menu-wrapper">
                    <a id="{{ #edit }}" href="{% url admin_edit_rsc id=s_id %}" title="{_ Edit _}">
                        {% catinclude "_rsc_edge_item.tpl" s_id %}
                    </a>
                </div>
            </li>
        {% endfor %}
    </ul>

    <div class="form-group">
       <a class="btn btn-default btn-sm" href="{% url admin_edges qhasobject=id %}"><i class="glyphicon glyphicon-list"></i> {_ View all referrers _}</a>
    </div>
{% endblock %}
