<div class="control-group survey-truefalse question-{{ nr }}">
    <label class="control-label">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div class="controls">
        <label class="radio inline">
            <input type="radio" id="{{ #yes }}" name="{{ blk.name}}" {% if answers[blk.name] == "true" %}checked="checked"{% endif %} value="1" /> {{ blk.yes|default:_"True" }}
        </label>
        <label class="radio inline">
            <input type="radio" id="{{ #no }}" name="{{ blk.name}}" {% if answers[blk.name] == "false" %}checked="checked"{% endif %} value="0" /> {{ blk.yes|default:_"False" }}
        </label>
    </div>
</div>
{% if blk.is_required %}
    {% validate id=#yes name=blk.name type={presence} %}
{% endif %}
