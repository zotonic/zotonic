{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
    <div class="control-group survey-long-answer">
        <label class="control-label">{{ blk.prompt }}</label>
        <blockquote>{{ result.answers[blk.name].answer|force_escape|linebreaksbr }}</blockquote>
    </div>
{% else %}
    <div class="control-group survey-long-answer question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
        <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
        {% if blk.explanation %}
             <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
        {% endif %}
        <div>
            <textarea type="text" class="form-control" rows="4" name="{{ blk.name }}" id="{{ #id }}">{{ answers[blk.name]|escape }}</textarea>
        </div>
    </div>
    {% if blk.is_required %}
        {% validate id=#id name=blk.name type={presence} %}
    {% endif %}
{% endif %}
