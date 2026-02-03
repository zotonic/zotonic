{% with answer_id|default:q.answer_id|to_integer as answer_id %}
{% with m.survey.did_survey[id] as did_survey %}
{% with m.survey.is_max_results_reached[id] as is_max_results_reached %}

{% if id.survey_is_disabled and not id.is_editable %}
    <p><em>{_ Closed _}</em></p>
{% elseif answer_id and id.is_editable %}
    {# Change existing answer from any user by an editor/admin #}
    {% wire
        postback={survey_start
            id=id
            answer_id=answer_id
            viewer=viewer
            element_id=element_id|default:"survey-question"
        }
        delegate="mod_survey"
    %}
{% elseif id.survey_multiple == 1 and not is_max_results_reached %}
    {# Multiple entry form and still available. #}
    {% include "_survey_start_button.tpl"
                id=id
                answers=answers
                viewer=viewer
                element_id=element_id|default:"survey-question"
    %}
{% elseif id.survey_multiple == 2 and did_survey and m.acl.user %}
    {# Single entry form and previous answers are allowed to be changed by a logged in user #}
    <p><span class="fa fa-info-circle"></span>
    {_ You already filled this in, but you can change your previous answers. _}</p>
    {% include "_survey_start_button.tpl"
                id=id
                answers=answers|default:m.survey.did_survey_answers[id]
                viewer=viewer
                is_autostart=is_autostart
                element_id=element_id|default:"survey-question"
    %}
{% elseif did_survey and id.survey_multiple /= 1 %}
    {# Previously filled in by this user/browser. #}
    <p class="alert alert-info">
        <span class="fa fa-exclamation-triangle"></span>
        {_ You already filled this in. _}
    </p>

    {% if viewer == 'overlay' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{_ Close _}</button>
        {% wire id=#survey_close
                action={overlay_close}
        %}
    {% elseif viewer == 'dialog' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{_ Close _}</button>
        {% wire id=#survey_close
                action={dialog_close}
        %}
    {% endif %}
{% elseif is_max_results_reached %}
    {# Maximum number of submissions has been reached. #}
    <p class="alert alert-info">
        <span class="fa fa-exclamation-triangle"></span>
        {_ The maximum number of submissions has been reached, you cannot fill this in anymore. _}
    </p>

    {% if viewer == 'overlay' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{_ Close _}</button>
        {% wire id=#survey_close
                action={overlay_close}
        %}
    {% elseif viewer == 'dialog' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{_ Close _}</button>
        {% wire id=#survey_close
                action={dialog_close}
        %}
    {% endif %}
{% elseif id|survey_is_save_intermediate and m.survey_saved.has_saved[id] %}
    {# Single entry form with intermediate saved results can be continued. #}
    <p><span class="fa fa-info-circle"></span>
    {_ You started filling this in and will continue where you left off. _}</p>
    {% include "_survey_start_button.tpl"
                id=id
                answers=answers|default:m.survey.did_survey_answers[id]
                viewer=viewer
                is_autostart=is_autostart
                is_survey_saved
                element_id=element_id|default:"survey-question"
    %}
{% else %}
    {# Show start button or start with the first page. #}
    {% include "_survey_start_button.tpl"
                id=id
                answers=answers
                viewer=viewer
                is_autostart=is_autostart
                element_id=element_id|default:"survey-question"
    %}
{% endif %}

{% endwith %}
{% endwith %}
{% endwith %}

{% if m.survey.is_allowed_results_download[id] %}
    <p>
        <a href="{% url survey_results id=id %}">{_ Show results (admin only) _}</a>
    </p>
{% endif %}
