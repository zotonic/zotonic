{% include "_survey_block_name_check.tpl" %}
<div class="control-group survey-long-answer question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label" for="{{ #id }}">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div>
        <textarea type="text" class="col-lg-6 col-md-6 form-control" rows="4" name="{{ blk.name }}" id="{{ #id }}">{{ answers[blk.name]|escape }}</textarea>
    </div>
</div>
{% if blk.is_required %}
    {% validate id=#id name=blk.name type={presence} %}
{% endif %}

