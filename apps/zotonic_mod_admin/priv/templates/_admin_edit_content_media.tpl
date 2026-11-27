{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editing media/file content #}

{% block widget_title %}
{_ File / media content _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-content-media{% endblock %}

{% block widget_attrs %}
    {% if id.is_editable and m.modules.active.mod_fileuploader %}
        data-fileuploader="{{ %{ id: id.id, is_medium: true }|escape }}"
        data-fileuploader-single
        data-fileuploader-confirm="{_ Replace the current media file? _}"
    {% endif %}
{% endblock %}

{% block widget_content %}
    {% if id.is_editable and m.modules.active.mod_fileuploader %}
        <p class="help-block fileuploader-drag-help">{_ Drag a file anywhere into this panel to replace the current media file. _}</p>
        <p class="fileuploader-status" role="status" aria-live="polite"></p>
    {% endif %}
    <div id="media-edit-view">
        {# show the media in the best possible way - updates on medium and rsc changes #}
        {% live template="_admin_edit_media_all.tpl"
                id=id
                topic=id
                topic=[ "bridge", "origin", "model", "media", "event", id, "update" ]
        %}
    </div>
{% endblock %}
