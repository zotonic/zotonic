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
    {% if m.search[{query hasobject=id sort="-seq" pagelen=10}] as incoming %}
        <p class="help-block">{_ Newest incoming connections. _}</p>

        <ul class="tree-list connections-list">
            {% for s_id in incoming %}
                {% with forloop.counter as index %}
                <li id="{{ #unlink_wrapper }}" class="menu-item">
                    <div class="menu-wrapper">
                        <a id="{{ #edit.index }}" href="{% url admin_edit_rsc id=s_id %}" title="{_ Edit _}">
                            {% catinclude "_rsc_edge_item.tpl" s_id %}
                        </a>
                        {% wire id=#edit.index
                            action={dialog_edit_basics id=s_id}
                        %}
                    </div>
                </li>
                {% endwith %}
            {% endfor %}
        </ul>

        {% with m.search.count::%{ hasobject: id } as count %}
        {% if count.result[1] > 10 %}
            <p class="help-block">
                {% trans "And {n} more." n=count.result[1]-10 %}
            </p>
        {% endif %}
        {% endwith %}
    {% else %}
        <p class="help-block">{_ There are no incoming connections. _}</p>
    {% endif %}

    <div class="form-group">
       <a class="btn btn-default btn-sm" href="{% url admin_edges qhasobject=id %}"><i class="glyphicon glyphicon-list"></i> {_ View all referrers _}</a>
    </div>
{% endblock %}
