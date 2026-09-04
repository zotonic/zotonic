{% with m.rsc[id].id as survey_id %}
{% with answer_id|to_integer as answer_id %}
{% if survey_id.is_editable and answer_id %}
    {% with m.survey.get_result[survey_id][answer_id] as result %}
        {% if result %}
            {% include "_admin_survey_editor_result_status.tpl"
                id=survey_id
                answer_id=answer_id
                result=result
                status_labels=status_labels
            %}
        {% endif %}
    {% endwith %}
{% endif %}
{% endwith %}
{% endwith %}
