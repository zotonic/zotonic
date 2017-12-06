{% if id.survey_is_autostart %}
    {% wire
        postback={survey_start id=id answers=answers}
        delegate="mod_survey"
    %}
{% else %}
    <p class="buttons survey-start clearfix">
        <button id="{{ #survey_next }}" class="btn btn-primary">{_ Start _}</button>
        {% wire id=#survey_next
            postback={survey_start id=id answers=answers}
            delegate="mod_survey"
        %}
    </p>
{% endif %}
