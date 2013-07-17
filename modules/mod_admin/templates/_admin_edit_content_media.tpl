{% extends "admin_edit_widget_std.tpl" %}

{# Widget for viewing/editing media/file content #}

{% block widget_title %}{_ File / media content _}{% endblock %}
{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-content-media{% endblock %}

{% block widget_content %}
    {% with id.medium  as  medium %}
        <div id="media-edit-view">
            {# show the media in the best possible way - separate template because we need to refresh this after an media replace #}
            {% all include "_admin_edit_media.tpl" medium=id.medium %}
        </div>
    {% endwith %}
{% endblock %}
