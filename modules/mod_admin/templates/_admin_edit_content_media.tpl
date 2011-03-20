{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editing media/file content #}

{% block widget_title %}{_ File / media content _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
{% with m.rsc[id].medium  as  medium %}
<div id="media-edit-view">
    {% include "_admin_edit_media_view.tpl" id=id languages=languages %}
</div>

{% button   text=_"Replace this media item" 
	    action={dialog_media_upload id=id action={update target="media-edit-view" template="_admin_edit_media_view.tpl" id=id}} 
	    disabled=not is_editable %}
	    
{% endwith %}
{% endblock %}
