{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey #}

{% block widget_title %}
{_ Results _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}content-survey{% endblock %}

{% block widget_before %}
{% lib "css/admin_survey.css" %}
{% endblock %}

{% block widget_content %}
    <p>
        {% if m.survey.is_allowed_results_download[id] and m.modules.active.mod_export %}
            <a id="{{ #download1 }}" class="btn btn-default" href="{% url survey_results_download type='csv' id=id %}">{_ Download CSV _}</a>
            {% wire id=#download1 propagate
                    action={alert text=_"Download will start in the background. Please check your download window."}
            %}
            <a id="{{ #download2 }}" class="btn btn-default" href="{% url survey_results_download type='xlsx' id=id %}">{_ Download Excel _}</a>
            {% wire id=#download2 propagate
                    action={alert text=_"Download will start in the background. Please check your download window."}
            %}
        {% endif %}

        <a class="btn btn-default" href="{% url survey_results id=id %}" target="_blank">{_ Show results _} <i class="fa fa-external-link"></i></a>
        <a class="btn btn-default" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
        {% wire id=#email_addresses postback={admin_show_emails id=id} delegate="mod_survey" %}
        <a class="btn btn-default" href="{% url survey_results_printable id=id %}" target="_blank">{_ Printable list _} <i class="fa fa-external-link"></i></a>
        <a class="btn btn-default" href="{% url admin_survey_editor id=id %}">{_ Results editor _}</a>
    </p>
{% endblock %}
