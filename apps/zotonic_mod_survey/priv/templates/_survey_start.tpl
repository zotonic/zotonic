{% with answer_id|default:q.answer_id|to_integer as answer_id %}
{% with m.survey.did_survey[id] as did_survey %}
{% with m.survey.is_max_results_reached[id] as is_max_results_reached %}

{% if id.survey_is_disabled and not id.is_editable %}
    <p><em>{% block survey_start_text_closed %}{_ Closed _}{% endblock %}</em></p>
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
{% elseif id|survey_is_save_intermediate and m.survey_saved.has_saved[id] and not is_max_results_reached %}
    {# Single entry form with intermediate saved results that will be continued. #}
    <p>
        <span class="fa fa-info-circle"></span>
        {% block survey_start_text_continue %}{_ You started filling this in and will continue where you left off. _}{% endblock %}
    </p>
    {% include "_survey_start_button.tpl"
                id=id
                answers=answers|default:m.survey.did_survey_answers[id]
                viewer=viewer
                is_autostart=is_autostart
                is_survey_saved=true
                element_id=element_id|default:"survey-question"
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
    <p>
        <span class="fa fa-info-circle"></span>
        {% block survey_start_text_change %}{_ You already filled this in, but you can change your previous answers. _}{% endblock %}
    </p>
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
        {% block survey_start_text_already_filled %}{_ You already filled this in. _}{% endblock %}
    </p>

    {% if viewer == 'overlay' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{% block survey_start_text_close_already_filled_overlay %}{_ Close _}{% endblock %}</button>
        {% wire id=#survey_close
                action={overlay_close}
        %}
    {% elseif viewer == 'dialog' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{% block survey_start_text_close_already_filled_dialog %}{_ Close _}{% endblock %}</button>
        {% wire id=#survey_close
                action={dialog_close}
        %}
    {% endif %}
{% elseif is_max_results_reached %}
    {# Maximum number of submissions has been reached. #}
    <p class="alert alert-info">
        <span class="fa fa-exclamation-triangle"></span>
        {% block survey_start_text_max_results %}{_ The maximum number of submissions has been reached, you cannot fill this in anymore. _}{% endblock %}
    </p>

    {% if viewer == 'overlay' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{% block survey_start_text_close_max_results_overlay %}{_ Close _}{% endblock %}</button>
        {% wire id=#survey_close
                action={overlay_close}
        %}
    {% elseif viewer == 'dialog' %}
        <button id="{{ #survey_close }}" class="btn btn-lg btn-default">{% block survey_start_text_close_max_results_dialog %}{_ Close _}{% endblock %}</button>
        {% wire id=#survey_close
                action={dialog_close}
        %}
    {% endif %}
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

{% with m.survey.is_allowed_results_download[id] as is_download %}
{% if id.is_editable or is_download %}
    <p>
        <span class="text-muted">{% block survey_start_text_admin_only %}{_ Admin only _}{% endblock %}:</span>
        {% if is_download %}
            <a href="{% url survey_results id=id %}">{% block survey_start_text_show_results %}{_ Show results _}{% endblock %}</a>
        {% endif %}
        {% if id.is_editable %}
            {% if is_download %}
                <span class="text-muted">|</span>
            {% endif %}
            {% if {admin_frontend_edit id=id}|url as url %}
                <a href="{{ url }}">{% block survey_start_text_edit_frontend %}{_ Edit _}{% endblock %}</a>
            {% else %}
                <a href="{% url admin_edit_rsc id=id %}">{% block survey_start_text_edit_admin %}{_ Edit _}{% endblock %}</a>
            {% endif %}
        {% endif %}
    </p>
{% endif %}
{% endwith %}

{# Close open survey entry forms in other tabs than the one where the form has been submitted. #}
{% if m.acl.user %}
    {% javascript %}
        cotonic.broker.subscribe(
            "bridge/origin/user/{{ m.acl.user }}/survey-submission/{{ id }}",
            function(msg) {
                const $form = $('.form-survey[data-id={{ id }}]');
                if (!$form.hasClass('masked')) {
                    setTimeout(() => {
                        $form.replaceWith('<p class="alert alert-info">{% block survey_start_text_submitted_other_tab %}{_ This form has been submitted in another tab, so your current session has been stopped. _}{% endblock %}</p>');
                    }, 500);
                }
            });
    {% endjavascript %}
{% endif %}
