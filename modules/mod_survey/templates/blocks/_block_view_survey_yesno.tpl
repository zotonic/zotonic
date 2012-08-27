<div class="control-group survey-yesno question-{{ nr }}">
    <label class="control-label">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div class="controls">
        <label class="radio inline">
            <input type="radio" id="{{ #yes }}" name="{{ blk.name}}" {% if answers[blk.name] == "yes" %}checked="checked"{% endif %} value="1" /> {{ blk.yes|default:_"Yes" }}
        </label>
        <label class="radio inline">
            <input type="radio" id="{{ #no }}" name="{{ blk.name}}" {% if answers[blk.name] == "no" %}checked="checked"{% endif %} value="0" /> {{ blk.yes|default:_"No" }}
        </label>
    </div>
</div>
{% if blk.is_required %}
    {% validate id=#yes name=blk.name type={presence} %}
{% endif %}

