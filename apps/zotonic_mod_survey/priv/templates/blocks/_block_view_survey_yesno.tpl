{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
    <div class="control-group survey-yesno">
        <label class="control-label">{{ blk.prompt }}</label>
        <b>
            {% if result.answers[blk.name].answer == 'yes' %}{{ blk.yes|default:_"True" }}
            {% elseif result.answers[blk.name].answer == 'no' %}{{ blk.no|default:_"False" }}
            {% else %}-
            {% endif %}
        </b>
    </div>
{% else %}
<div class="form-group survey-yesno question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label">{{ blk.prompt }}</label>
{% if blk.explanation %}
     <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
    <div>
{% if blk.input_type == 'submit' %}
        <button id="{{ #yes }}" name="{{ blk.name}}" class="btn btn-default" type="submit" value="1">
            <span></span>{{ blk.yes|default:_"Yes" }}
        </button>
        <button id="{{ #no }}" name="{{ blk.name}}" class="btn btn-default" type="submit" value="0">
            <span></span>{{ blk.no|default:_"No" }}
        </button>
{% else %}
        <label class="radio-inline">
            <input type="radio" id="{{ #yes }}" name="{{ blk.name}}" {% if answers[blk.name] == "yes" %}checked="checked"{% endif %} value="1" /> {{ blk.yes|default:_"Yes" }}
        </label>
        <label class="radio-inline">
            <input type="radio" id="{{ #no }}" name="{{ blk.name}}" {% if answers[blk.name] == "no" %}checked="checked"{% endif %} value="0" /> {{ blk.no|default:_"No" }}
        </label>
        {% if blk.is_required %}
            {% validate id=#yes name=blk.name type={presence} %}
        {% endif %}
{% endif %}
    </div>
</div>
{% endif %}
