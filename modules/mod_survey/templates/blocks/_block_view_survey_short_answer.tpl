<div class="control-group survey-short-answer">
    <label for="{{ #id }}">{{ blk.prompt }}</label>
    <div class="controls">
        <input type="text" class="span6" name="{{ blk.name }}" id="{{ #id }}" value="{{ answers[blk.name]|escape }}" />
    </div>
</div>
{% if blk.is_required %}
    {% if blk.validation == "email" %}
        {% validate id=#id name=blk.name type={presence} type={email} %}
    {% elseif blk.validation == "numericality" %}
        {% validate id=#id name=blk.name type={presence} type={numericality} %}
    {% else %}
        {% validate id=#id name=blk.name type={presence} %}
    {% endif %}
{% else %}
    {% if blk.validation == "email" %}
        {% validate id=#id name=blk.name type={email} %}
    {% elseif blk.validation == "numericality" %}
        {% validate id=#id name=blk.name type={numericality} %}
    {% endif %}
{% endif %}
