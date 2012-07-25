{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Attached media _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-depiction{% endblock %}

{% block widget_content %}
{% if is_editable or m.rsc[id].depiction %}
<div id="links-{{ id }}-depiction" data-reload-template="_edit_media.tpl">
	{% include "_edit_media.tpl" id=id %}
</div>
{% if is_editable %}
<div class="widget-footer">
    <div class="pull-right">
            <a class="btn" id="{{ #connect }}" href="#connect"><i class="icon icon-camera"></i> {_ add media item _}</a>
            {% wire id=#connect 
                    action={dialog_open template="_action_dialog_connect.tpl" 
                                title=[_"Add a connection: ", m.rsc.depiction.title] 
                                subject_id=id
                                edge_template="_rsc_edge_media.tpl"
                                predicate="depiction"
                                actions=[
                                    {postback postback={reload_media rsc_id=id div_id=["links-",id|make_list,"-depiction"]}
                                              delegate="resource_admin_edit"}
                                ]}
            %}
    </div>
</div>
{% endif %}

{% endif %}
{% endblock %}
