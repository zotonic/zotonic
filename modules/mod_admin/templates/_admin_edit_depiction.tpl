{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Attached media _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}


{% block widget_content %}
{% if is_editable or m.rsc[id].depiction %}
<div id="{{ #media }}">
	{% catinclude "_edit_media.tpl" id media=media div_id=#media %}
</div>
<div class="clear">
	{% if is_editable %}
		{% button   text=_"add a new media item"
			    action={dialog_media_upload subject_id=id stay
			    action={postback postback={reload_media rsc_id=id div_id=#media} delegate="resource_admin_edit"}}
		%}

		{% button   text=_"add existing media item"
			    action={dialog_link subject_id=id predicate="depiction"
						action={postback
						postback={reload_media rsc_id=id div_id=#media}
						delegate="resource_admin_edit"}
				    } 
			    %}
	{% else %}
		&nbsp;
	{% endif %}
</div>
{% endif %}
{% endblock %}
