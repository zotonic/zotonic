{% include "_survey_block_name_check.tpl" %}
<div class="form-group survey-button question-{{ nr }}">
{% if blk.explanation %}
    <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <button class="btn btn-default {{ blk.style }}" id="{{ #id }}" name="{{ blk.name }}" type="submit">{{ blk.prompt }}</button>
    <input type="hidden" value="{{ blk.name }}" name="survey$button" />
</div>
