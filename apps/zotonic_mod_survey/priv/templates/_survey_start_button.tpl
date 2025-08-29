{% if is_autostart or id.survey_is_autostart %}
    {% survey_start
            id=id
            answers=answers
            answer_user_id=answer_user_id
            viewer=viewer
            element_id=element_id|default:"survey-question"
    %}
{% else %}
    <p class="buttons survey-start clearfix">
        <button id="{{ #survey_next }}" class="btn btn-lg btn-primary">{{ start_button_text|default:_"Start" }}</button>
        {% wire id=#survey_next
            postback={survey_start
                id=id
                answers=answers
                answer_user_id=answer_user_id
                viewer=viewer
                element_id=element_id|default:"survey-question"
            }
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
        {% else %}
            <a href="#" id="{{ #close }}" style="display:none" class="btn btn-lg btn-default">{_ Close _}</a>
            <a href="#" id="{{ #back_history }}" style="display:none" class="btn btn-lg btn-default">{_ Back _}</a>
            {% javascript %}
                if (window.opener) {
                    $('#{{ #close }}')
                        .show()
                        .click(function() {
                            try {
                                window.opener.z_reload();
                            } catch (_E) { };
                            window.close();
                            return false;
                        });
                } else if (window.history.length > 1) {
                    $('#{{ #back_history }}')
                        .show()
                        .click(function() {
                            window.history.go(-1);
                            return false;
                        });
                }
            {% endjavascript %}
        {% endif %}
    </p>
{% endif %}
