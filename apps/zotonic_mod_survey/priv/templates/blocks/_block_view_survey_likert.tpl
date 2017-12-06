{% include "_survey_block_name_check.tpl" %}
{% if is_survey_answer_view %}
    <div class="control-group survey-likert">
        <label class="control-label">{{ blk.prompt }}</label>
        {% with result.answers[blk.name].answer as ans %}
            <div class="controls">
                <span>
                    {{ blk.disagree|default:_"Strongly Disagree" }}
                </span>
                &nbsp;
                {% for a in [1,2,3,4,5] %}
                    {% if ans == a %}<span>&#x25C9;</span>
                    {% else %}<span>&#x25EF;</span>
                    {% endif %}
                    &nbsp;
                {% endfor %}
                <span>
                    {{ blk.agree|default:_"Strongly Agree" }}
                </span>
            </div>
        {% endwith %}
    </div>
{% else %}
    {% with answers[blk.name] as value %}
    <div class="form-group survey-likert question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
        <label class="control-label">{{ blk.prompt }}</label>
        {% if blk.explanation %}
             <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
        {% endif %}
        <div>
            <label class="radio-inline">
                {{ blk.disagree|default:_"Strongly Disagree" }}
            </label>
            <label class="radio-inline"><input id="{{ #q1 }}" type="radio" name="{{blk.name}}" value="1" {% if value=="1" %}checked="checked"{% endif %} /> </label>
            <label class="radio-inline"><input id="{{ #q2 }}" type="radio" name="{{blk.name}}" value="2" {% if value=="2" %}checked="checked"{% endif %} /> </label>
            <label class="radio-inline"><input id="{{ #q3 }}" type="radio" name="{{blk.name}}" value="3" {% if value=="3" %}checked="checked"{% endif %} /> </label>
            <label class="radio-inline"><input id="{{ #q4 }}" type="radio" name="{{blk.name}}" value="4" {% if value=="4" %}checked="checked"{% endif %} /> </label>
            <label class="radio-inline"><input id="{{ #q5 }}" type="radio" name="{{blk.name}}" value="5" {% if value=="5" %}checked="checked"{% endif %} /> </label>
            <label class="radio-inline">
                {{ blk.agree|default:_"Strongly Agree" }}
            </label>
        </div>
    </div>
    {% endwith %}
    {% if blk.is_required %}
        {% validate id=#q1 name=blk.name type={presence} %}
    {% endif %}
{% endif %}
