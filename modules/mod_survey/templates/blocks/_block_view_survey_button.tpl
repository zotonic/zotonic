<div class="control-group survey-button">
{% if blk.explanation %}
    <p class="help-block">{{ blk.explanation }}</p>
{% endif %}
    <button class="btn {{ blk.style }}" id="{{ #id }}" name="{{ blk.name }}" type="submit">{{ blk.prompt }}</button>
    <input type="hidden" value="{{ blk.name }}" name="survey$button" />
</div>
