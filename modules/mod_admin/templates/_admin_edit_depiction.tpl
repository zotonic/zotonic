{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Attached media _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-depiction{% endblock %}

{% block widget_content %}
{% if is_editable or m.rsc[id].depiction %}
<div id="{{ #media }}">
	{% catinclude "_edit_media.tpl" id media=media div_id=#media %}
</div>
{% if is_editable %}
<div class="widget-footer">
    <div class="pull-right">
        {% button   text=_"add a new media item"
            class="btn"
            icon="icon-camera"
            action={dialog_media_upload subject_id=id stay
            action={postback postback={reload_media rsc_id=id div_id=#media} delegate="resource_admin_edit"}}
        %}

        {% button   text=_"add existing media item"
            class="btn"
            icon="icon-folder-open"
            action={dialog_link subject_id=id predicate="depiction"
            action={postback
            postback={reload_media rsc_id=id div_id=#media}
            delegate="resource_admin_edit"}
            } 
        %}
    </div>
</div>
{% endif %}

{% endif %}
{% endblock %}
