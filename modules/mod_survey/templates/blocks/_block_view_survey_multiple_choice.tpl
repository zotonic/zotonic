{% include "_survey_block_name_check.tpl" %}

<div class="control-group survey-short-answer question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
    {% if blk.explanation %}
        <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
    {% endif %}
    <div>

        <select class="col-lg-6 col-md-6 form-control" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}">
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
