{% with answers[blk.name] as value %}
<div class="control-group survey-likert question-{{ nr }}">
    <label class="control-label">{{ blk.prompt }}</label>

{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}

    <div class="controls">
        <label class="radio inline">
            {{ blk.disagree|default:_"Strongly Disagree" }}
        </label>
        <label class="radio inline"><input id="{{ #q1 }}" type="radio" name="{{blk.name}}" value="1" {% if value=="1" %}checked="checked"{% endif %} /> </label>
        <label class="radio inline"><input id="{{ #q2 }}" type="radio" name="{{blk.name}}" value="2" {% if value=="2" %}checked="checked"{% endif %} /> </label>
        <label class="radio inline"><input id="{{ #q3 }}" type="radio" name="{{blk.name}}" value="3" {% if value=="3" %}checked="checked"{% endif %} /> </label>
        <label class="radio inline"><input id="{{ #q4 }}" type="radio" name="{{blk.name}}" value="4" {% if value=="4" %}checked="checked"{% endif %} /> </label>
        <label class="radio inline"><input id="{{ #q5 }}" type="radio" name="{{blk.name}}" value="5" {% if value=="5" %}checked="checked"{% endif %} /> </label>
        <label class="radio inline">
            {{ blk.agree|default:_"Strongly Agree" }}
        </label>
    </div>
</div>
{% endwith %}
{% if blk.is_required %}{% validate id=#q1 name=blk.name type={presence} %}{% endif %}
