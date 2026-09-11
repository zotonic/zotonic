{% extends "admin_edit_widget_std.tpl" %}

{# Widget for showing the most recent incoming connections to a rsc #}

{% block widget_title %}
{_ Recent referrers _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
            %{
                title: _"Page referrers",
                text: _"This page can be connected from other pages. For example it could be an author of an article."
            }|escape
        }}" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-referrers{% endblock %}

{% block widget_content %}
    <div class="form-group">
        <a class="btn btn-default btn-sm" href="{% url admin_edges qhasobject=id %}">
            →○ {_ View all referrers _}
        </a>
        <button id="{{ #connect_subject }}" type="button" class="btn btn-default btn-sm">
            + {_ Add connection _}
        </button>
        {% wire id=#connect_subject
            action={dialog_open
                template="_action_dialog_connect_select_predicate.tpl"
                title=_"Add an incoming connection"
                object_id=id
                center=0
                width="large"
            }
        %}
    </div>

    <div id="{{ #referrers_undo }}"></div>

    {% live template="_admin_edit_content_page_referrers_list.tpl"
        topic={subject id=id}
        id=id
        undo_message_id=#referrers_undo
    %}
{% endblock %}
