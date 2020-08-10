{% if id.survey_is_autostart %}
    {% wire
        postback={survey_start id=id answers=answers}
        delegate="mod_survey"
    %}
{% else %}
    <p class="buttons survey-start clearfix">
        <button id="{{ #survey_next }}" class="btn btn-lg btn-primary">{{ start_button_text|default:_"Start" }}</button>
        {% wire id=#survey_next
            postback={survey_start id=id answers=answers}
            delegate="mod_survey"
        %}
    </p>
{% endif %}
