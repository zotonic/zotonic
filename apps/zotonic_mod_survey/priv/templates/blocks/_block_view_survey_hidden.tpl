{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
    <div class="form-group survey-short-answer">
        <label class="control-label">{{ blk.name }}</label>
        <blockquote>{{ result.answers[blk.name].answer|escape }}</blockquote>
    </div>
{% else %}
    <input type="hidden" name="{{ blk.name }}" value="{{ blk.value }}">
{% endif %}
