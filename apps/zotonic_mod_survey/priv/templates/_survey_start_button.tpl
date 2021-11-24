{% if id.survey_is_autostart %}
    {% wire
        postback={survey_start id=id answers=answers viewer=viewer}
        delegate="mod_survey"
    %}
{% else %}
    <p class="buttons survey-start clearfix">
        <button id="{{ #survey_next }}" class="btn btn-lg btn-primary">{{ start_button_text|default:_"Start" }}</button>
        {% wire id=#survey_next
            postback={survey_start id=id answers=answers viewer=viewer}
            delegate="mod_survey"
        %}

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
    </p>
{% endif %}
