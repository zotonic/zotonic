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
                <div class="form-group">
                    <label>{_ Status _}</label>
                    <div class="survey-status-options">
                        <label class="survey-status-option">
                            <input type="radio"
                                   name="status"
                                   value=""
                                   aria-label="{_ No status _}"
                                   {% if result.status|is_undefined %}checked{% endif %}>
                            <span class="survey-status-label survey-status-label-none" aria-hidden="true"></span>
                        </label>
                        {% for status_index in [0, 1, 2, 3, 4, 5] %}
                            {% with status_labels[forloop.counter] as status_label %}
                                <label class="survey-status-option">
                                    <input type="radio"
                                           name="status"
                                           value="{{ status_index }}"
                                           {% if status_label %}
                                               aria-label="{{ status_label|escape }}"
                                           {% else %}
                                               aria-label="{% trans "Status {status}" status=status_index %}"
                                           {% endif %}
                                           {% if result.status == status_index %}checked{% endif %}>
                                    <span class="survey-status-label survey-status-label-{{ status_index }}" aria-hidden="true"></span>
                                </label>
                            {% endwith %}
                        {% endfor %}
                    </div>
                </div>

                <div class="form-group">
                    <label for="{{ #status_note }}">{_ Note _}</label>
                    {# Keep in sync with STATUS_NOTE_MAX_LENGTH in m_survey. #}
                    <textarea id="{{ #status_note }}"
                              name="status_note"
                              rows="5"
                              maxlength="65536"
                              class="form-control">{{ result.status_note|escape }}</textarea>
                    <p class="help-block">
                        {_ The status and note are only visible to people who can edit the survey. _}
                    </p>
                </div>

                {% if result.status_date %}
                    <p class="text-muted small">
                        {_ Last status change _}: {{ result.status_date|date:_"Y-m-d H:i" }}
                        {% if result.status_modifier_id %}
                            {_ by _} {% include "_name.tpl" id=result.status_modifier_id %}
                        {% endif %}
                    </p>
                {% endif %}

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
