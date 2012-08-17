<div class="control-group survey-long-answer">
    <label for="{{ #id }}">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation }}</p>
{% endif %}
    <div class="controls">
        <textarea type="text" class="span6" rows="4" name="{{ blk.name }}" id="{{ #id }}">{{ answers[blk.name]|escape }}</textarea>
    </div>
</div>
{% if blk.is_required %}
    {% validate id=#id name=blk.name type={presence} %}
{% endif %}

