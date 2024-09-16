{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Export _}
<div class="widget-header-tools">
    <a href="#" class="z-btn-help do_dialog" data-dialog="{{
        %{
            title: _"Help about export",
            text: _"Download this page or the query as a spreadsheet or in another format."
        }|escape
    }}"></a>
</div>
{% endblock %}

{% block widget_show_minimized %}true{% endblock %}
{% block widget_id %}admin_export_sidebar{% endblock %}

{% block widget_content %}
<div class="form-group">
    {% if id.is_a.query %}
        <p>{_ Download all the pages matching the query _}</p>

        <p>
            <a class="btn btn-default" href="{% url export_rsc_query type='csv' id=id %}">{_ Download CSV _}</a>
            <a class="btn btn-default" href="{% url export_rsc_query type='xlsx' id=id %}">{_ Download Excel _}</a>
            <a class="btn btn-default" href="{% url export_rsc_query type='vevent' id=id %}">{_ Download Event _}</a>
        </p>

        <p class="help-block">
            <span class="glyphicon glyphicon-info-sign"></span> {_ Download as Event if the query returns events and you want export for a calendar program. _}
        </p>
    {% elseif id.is_a.collection %}
        <p>{_ Download all the pages in the collection _}</p>

        <p>
            <a class="btn btn-default" href="{% url export_rsc_query type='csv' id=id %}">{_ Download CSV _}</a>
            <a class="btn btn-default" href="{% url export_rsc_query type='xlsx' id=id %}">{_ Download Excel _}</a>
            {% if id.o.haspart[1].is_a.event %}
                <a class="btn btn-default" href="{% url export_rsc_query type='vevent' id=id %}">{_ Download Event _}</a>
            {% endif %}
        </p>
    {% else %}
        <p>
            <a class="btn btn-default" href="{% url export_rsc type='csv' id=id %}">{_ Download CSV _}</a>
            <a class="btn btn-default" href="{% url export_rsc type='xlsx' id=id %}">{_ Download Excel _}</a>
            {% if id.is_a.event %}
                <a class="btn btn-default" href="{% url export_rsc type='vevent' id=id %}">{_ Download Event _}</a>
            {% endif %}
        </p>
    {% endif %}
</div>
{% endblock %}
