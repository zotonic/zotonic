<div id="{{ #result }}" class="survey-result-view-dialog">
    {% include "_survey_result_view.tpl"
        id=id
        answer_id=answer_id
        element_id=#result
        status_labels=status_labels
        action=action
        on_success=on_success
        extra_actions_template=extra_actions_template
    %}
</div>
