{#
    Read-only status and note for a survey answer.

    status_labels is an optional ordered list of label texts for status 0–5.
    Without a label, only the corresponding color swatch is shown.
#}
{% with m.rsc[id].id as survey_id %}
    {% if survey_id.is_editable %}
        {% with m.survey.get_result[survey_id][answer_id] as result %}
            {% if result.status|is_defined or result.status_note %}
                <div class="survey-answer-status">
                    {% if result.status|is_defined %}
                        {% for status_index in [0, 1, 2, 3, 4, 5] %}
                            {% if result.status == status_index %}
                                {% with status_labels[forloop.counter] as status_label %}
                                    <span class="survey-status-label survey-status-label-{{ status_index }}">
                                        {% if status_label %}
                                            {{ status_label|escape }}
                                        {% else %}
                                            <span class="sr-only">{% trans "Status {status}" status=status_index %}</span>
                                        {% endif %}
                                    </span>
                                {% endwith %}
                            {% endif %}
                        {% endfor %}
                    {% endif %}

                    {% if result.status_note %}
                        <div class="survey-status-note">
                            {{ result.status_note|escape|linebreaksbr }}
                        </div>
                    {% endif %}
                </div>
            {% endif %}
        {% endwith %}
    {% endif %}
{% endwith %}
