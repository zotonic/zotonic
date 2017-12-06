{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
    {% with result.answers[blk.name].answer as ans %}
        {% if ans == 'yes' %}
           <div class="control-group survey-button">
                <label class="control-label">
                    <span class="fa fa-check"></span> {{ blk.prompt }}
                </label>
            </div>
        {% endif %}
    {% endwith %}
{% else %}
    <div class="form-group survey-button question-{{ nr }}">
        {% if blk.explanation %}
            <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
        {% endif %}
        <button class="btn btn-default {{ blk.style }}" id="{{ #id }}" name="{{ blk.name }}" type="submit">{{ blk.prompt }}</button>
        <input type="hidden" value="{{ blk.name }}" name="survey$button" />
    </div>
{% endif %}
