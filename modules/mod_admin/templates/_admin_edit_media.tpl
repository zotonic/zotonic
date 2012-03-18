{% if medium.filename %}
<p>
    {{ medium.mime }} 
    {% if medium.width and medium.height %}
    &ndash; {{ medium.width }} x {{ medium.height }} {_ pixels _}
    {% endif %}
    &ndash; {{ medium.filename }}
    &ndash; {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
</p>

{% if medium.width and medium.height %}
<div class="edit-media">
    {% if medium.width < 590  %}
    {% media medium %}
    {% else %}
    {% media medium width=590 height=590 %}
    {% endif %}
</div>
{% endif %}

<div class="save-buttons">
    <div class="pull-right">
        <a class="btn" href="{% url media_attachment star=medium.filename %}" class="button">{_ download _}</a>
        {% button   text=_"Replace this media item"
            class="btn"
	    action={dialog_media_upload id=id action={update target="media-edit-view" template="_admin_edit_media_view.tpl" id=id}} 
	    disabled=not is_editable %}

    </div>
</div>
{% endif %}
