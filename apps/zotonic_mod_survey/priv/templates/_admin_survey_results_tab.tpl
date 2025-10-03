{% extends "admin_edit_widget_std.tpl" %}

{# Admin controls for the survey (form) #}


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
    <fieldset>
        <legend>{_ View _}</legend>
        <div>
            <a class="btn btn-default" href="{% url admin_survey_editor id=id %}">{_ Results editor _}</a>

            <a class="btn btn-default" href="{% url survey_results_printable id=id %}" target="_blank">{_ Printable list _} <i class="glyphicon glyphicon-new-window"></i></a>

            <a class="btn btn-default" href="{% url survey_results id=id %}" target="_blank">{_ Show charts _} <i class="glyphicon glyphicon-new-window"></i></a>
        </div>
        <p class="help-block">{_ View and edit results. _} {_ The charts show aggregated results from closed questions like thurstone or multiple choice and yes/no. _}</p>
    </fieldset>

    {% if m.survey.is_allowed_results_download[id] and m.modules.active.mod_export %}
        <fieldset>
            <legend>{_ Download _}</legend>
            <div>
                <a id="{{ #download1 }}" class="btn btn-default" href="{% url survey_results_download type='csv' id=id %}">{_ Download CSV _}</a>
                {% wire id=#download1 propagate
                        action={alert text=_"Download will start in the background. Please check your download window."}
                %}

                <a id="{{ #download2 }}" class="btn btn-default" href="{% url survey_results_download type='xlsx' id=id %}">{_ Download Excel _}</a>
                {% wire id=#download2 propagate
                        action={alert text=_"Download will start in the background. Please check your download window."}
                %}
            </div>
            <p class="help-block">{_ Download a spreadsheet with all answers. _}</p>
        </fieldset>
    {% endif %}

    <fieldset>
        <legend>{_ E-mail addresses _}</legend>
        <div>
            <a class="btn btn-default" href="#" id="{{ #email_addresses }}">{_ Show email addresses _}</a>
            {% wire id=#email_addresses postback={admin_show_emails id=id} delegate="survey_admin" %}

            {% if m.modules.active.mod_mailinglist and m.acl.use.mod_mailinglist %}
                <a class="btn btn-default" href="#" id="{{ #email_mailinglist }}">{_ Add to mailinglist _}</a>
                {% wire id=#email_mailinglist
                        action={dialog_open title=_"Add to mailinglist"
                                            template="_dialog_survey_mailinglist.tpl"
                                            id=id}
                %}
            {% endif %}
        </div>
        <p class="help-block">{_ Questions with the question label “email” can be exported. _}</p>
    </fieldset>
{% endblock %}
