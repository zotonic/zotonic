{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editing media/file content #}

{% block widget_title %}
{_ File / media content _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-content-media{% endblock %}

{% block widget_content %}
    <div id="media-edit-view">
        {# show the media in the best possible way - updates on medium and rsc changes #}
        {% live template="_admin_edit_media_all.tpl"
                id=id
                topic=id
                topic=[ "bridge", "origin", "model", "media", "event", id, "update" ]
        %}
    </div>
{% endblock %}
