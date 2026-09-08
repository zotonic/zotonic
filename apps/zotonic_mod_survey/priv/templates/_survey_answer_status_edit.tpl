{#
    Status editor for inclusion in a dialog.

    status_labels is an optional ordered list of label texts for status 0–5.
    Without a label, only the corresponding color swatch is shown.
#}
{% if id.is_editable %}
    {% with m.survey.get_result[id][answer_id] as result %}
        {% if result %}
            {% wire id=#form
                    type="submit"
                    postback={survey_answer_status
                        id=id
                        answer_id=answer_id
                        on_success=on_success
                    }
                    delegate=`mod_survey`
            %}
            <form id="{{ #form }}" method="POST" action="postback" class="form">
                {% include "_survey_answer_status_fields.tpl"
                    result=result
                    status_labels=status_labels
                %}

                <div class="modal-footer">
                    {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
                    {% button class="btn btn-primary" type="submit" text=_"Save" %}
                </div>
            </form>
        {% else %}
            <div class="alert alert-danger">{_ This survey answer could not be found. _}</div>
        {% endif %}
    {% endwith %}
{% else %}
    <div class="alert alert-danger">{_ You are not allowed to change the status. _}</div>
{% endif %}
