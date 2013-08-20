{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editing media/file content #}

{% block widget_title %}{_ File / media content _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-content-media{% endblock %}

{% block widget_content %}
    <div id="media-edit-view">
        {# show the media in the best possible way - separate template because we need to refresh this after an media replace #}
        {% include "_admin_edit_media_all.tpl" id=id %}
    </div>
{% endblock %}
