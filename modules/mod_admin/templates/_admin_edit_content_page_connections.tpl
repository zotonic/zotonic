{% extends "admin_edit_widget_std.tpl" %}

{# Widget for editing connections between rscs #}

{% block widget_title %}
{_ Page connections _}
<div class="widget-header-tools">
    <a href="javascript:void(0)" class="z-btn-help do_dialog" data-dialog="title: '{{ _"Page connections"|escapejs }}', text: '{{ _"This page is able to connect to others. For example you can connect it to an actor or a brand."|escapejs }}'" title="{_ Need more help? _}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-connections{% endblock %}

{% block widget_content %}
{% with m.rsc[id] as r %}
<div id="unlink-undo-message"></div>
{% with predicate_ids|default:r.predicates_edit as pred_shown %}
    {% for name, p in m.predicate %}
        {% if p.id|member:pred_shown %}
           {% ifnotequal name "depiction" %}
               <h4>{{ p.title }}</h4>

                {% live template="_admin_edit_content_page_connections_list.tpl"
                    topic={object id=id predicate=name}
                    id=id
                    predicate=name|as_atom
                    button_label=button_label
                    button_class=button_class
                    dialog_title_add=dialog_title_add
                    callback=callback
                    action=action
                    unlink_action=unlink_action
                    list_id=list_id
                    is_editable=is_editable
                %}
           
                <hr />
            {% endifnotequal %}
        {% endif %}
    {% endfor %}
{% endwith %}
{% if not hide_referrers %}
    <div class="form-group">
       <a class="btn btn-default btn-sm" href="{% url admin_referrers id=id %}"><i class="glyphicon glyphicon-list"></i> {_ View all referrers _}</a>
    </div>
{% endif %}
{% endwith %}
{% endblock %}