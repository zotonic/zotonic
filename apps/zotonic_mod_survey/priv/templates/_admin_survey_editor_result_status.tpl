{% with m.rsc[id].id as survey_id %}
{% if survey_id.is_editable and result.id == answer_id %}
    <td id="{{ #status_cell.answer_id }}"
        class="survey-status-cell{% if result.status|is_defined %} survey-status-label survey-status-label-{{ result.status }}{% endif %}">
        <button id="{{ #status_button.answer_id }}" type="button" title="{_ Edit status _}">
            {% if result.status_note %}
                {{ result.status_note|escape }}
            {% elseif result.status|is_undefined %}
                <span class="fa fa-pencil" aria-hidden="true"></span>
                <span class="sr-only">{_ Set status _}</span>
            {% else %}
                <span class="sr-only">{_ Edit status _}</span>
            {% endif %}
        </button>
        {% wire id=#status_button.answer_id
                action={dialog_open
                    template="_dialog_survey_answer_status.tpl"
                    title=_"Edit status"
                    id=survey_id
                    answer_id=result.id
                    on_success={replace
                        target=#status_cell.answer_id
                        template="_admin_survey_editor_result_status_update.tpl"
                        id=survey_id
                        answer_id=result.id
                    }
                }
        %}
    </td>
{% endif %}
{% endwith %}
