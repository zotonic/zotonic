{% if q.answer_id and id.is_editable %}
    {% wire
        postback={survey_start id=id answer_id=q.answer_id viewer=viewer}
        delegate="mod_survey"
    %}
{% elseif id.survey_multiple == 1 %}
    {% include "_survey_start_button.tpl" id=id answers=answers viewer=viewer%}
{% else %}
    {% with m.survey.did_survey[id] as did_survey %}
        {% if did_survey and id.survey_multiple == 2 %}
            <p><span class="fa fa-exclamation-triangle"></span> {_ You already filled this in, but you can change your previous answers. _}</p>
            {% include "_survey_start_button.tpl"
                        id=id
                        answers=answers|default:m.survey.did_survey_answers[id]
                        viewer=viewer
            %}
         {% elseif not did_survey or id.survey_multiple == 1 %}
            {% include "_survey_start_button.tpl" id=id answers=answers viewer=viewer %}
        {% else %}
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
		{% endif %}
    {% endwith %}
{% endif %}

{% if m.survey.is_allowed_results_download[id] %}
    <p>
        <a href="{% url survey_results id=id %}">{_ Show results (admin only) _}</a>
    </p>
{% endif %}
