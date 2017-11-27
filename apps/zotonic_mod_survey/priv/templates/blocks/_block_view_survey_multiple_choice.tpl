{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
<div class="control-group survey-short-answer">
    <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
    <p><b>{{ result.answers[blk.name].answer|escape|default:"-" }}</b></p>
</div>
{% else %}
<div class="form-group survey-short-answer question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
    {% if blk.explanation %}
        <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
    {% endif %}
    <div>
        <select class="form-control" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}">
            <option value="">{_ select… _}</option>
            {% for choice in blk.choices|split:"\n" %}
                <option value="{{ choice|escape }}">{{ choice|escape }}</option>
            {% endfor %}
        </select>
        {% if blk.is_required %}
            {% validate id=#id name=blk.name type={presence} %}
        {% endif %}
    </div>
</div>
{% endif %}
