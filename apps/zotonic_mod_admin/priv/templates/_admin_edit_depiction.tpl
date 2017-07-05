{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Attached media _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-depiction{% endblock %}

{% block widget_content %}
    {% if is_editable or m.rsc[id].depiction %}
        <div id="links-{{ id }}-depiction" data-reload-template="_edit_media.tpl">
	        {% include "_edit_media.tpl" id=id %}
        </div>
        {% if is_editable %}
            <div class="form-group clearfix">
                <div class="pull-right">
                    <a class="btn btn-default" id="{{ #connect }}" href="#connect"><i class="icon glyphicon glyphicon-camera"></i> {_ add media item _}</a>
                    {% wire id=#connect
                        action={dialog_open
                            template="_action_dialog_connect.tpl"
                            title=[_"Add a connection: ", m.rsc.depiction.title]
                            subject_id=id
                            edge_template="_rsc_edge_media.tpl"
                            predicate=`depiction`
                            tab="upload"
                            tabs_enabled=tabs_enabled
                            actions=[
                                {postback postback={reload_media rsc_id=id div_id=["links-",id|make_list,"-depiction"]}
                                    delegate="controller_admin_edit"}
                            ]
                            center=0
                        }
                    %}
                </div>
            </div>
        {% endif %}

    {% endif %}
{% endblock %}
