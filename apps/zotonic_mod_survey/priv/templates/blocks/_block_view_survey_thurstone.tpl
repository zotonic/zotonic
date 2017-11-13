{% include "_survey_block_name_check.tpl" %}

{% if is_survey_answer_view %}
    {% with blk|survey_prepare_thurstone:0 as props %}
        <div class="control-group survey-thurstone">
            <label class="control-label">{{ blk.prompt }}</label>
            {% with result.answers[blk.name].answer as ans %}
            <ul>
                {% for option in props.answers %}
                    {% if blk.is_test %}
                        {% if option.value|member:ans and option.is_correct %}
                            <li class="survey-test-feedback survey-q-ok">
                                <span class='survey-test-feedback-icon'>
                                    &#10004;
                                    {{ option.option }}
                                </span>
                            </li>
                        {% elseif option.value|member:ans and not option.is_correct %}
                            <li class="survey-test-feedback survey-q-not-ok">
                                <span class='survey-test-feedback-icon'>
                                    &#10006;
                                    {{ option.option }}
                                </span>
                            </li>
                        {% else %}
                            <li>{{ option.option }}</li>
                        {% endif %}
                    {% elseif option.value|member:ans %}
                        <li><b>{{ option.option }}</b></li>
                    {% else %}
                        <li>{{ option.option }}</li>
                    {% endif %}
                {% endfor %}
            </ul>

            {% if blk.test_correct and ans|survey_any_correct_answer:props %}
                <p class="help-block survey-test-feedback-correct">
                    {{ blk.test_correct }}
                </p>
            {% endif %}
            {% if blk.test_wrong and ans|survey_any_wrong_answer:props %}
                <p class="help-block survey-test-feedback-wrong">
                    {{ blk.test_wrong }}
                </p>
            {% endif %}

            {% endwith %}
        </div>
    {% endwith %}
{% elseif blk.is_test %}
    {% include "blocks/_block_view_survey_thurstone_test.tpl" %}
{% else %}
    {% with blk|survey_prepare_thurstone as props %}
    {% with answers[blk.name]|survey_answer_split:blk as ans %}
    <div class="control-group survey-thurstone type-{{ blk.input_type|default:'single' }} question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
        <label class="control-label">{{ blk.prompt }}</label>
        <div class="controls">
    {% if blk.input_type == 'multi' %}
        {% for option in props.answers %}
            {% with forloop.counter as index %}
                <label class="checkbox">
                    <input id="{{ #thur.index }}" name="{{ blk.name }}" type="checkbox" value="{{ option.value }}" {% if option.value|member:ans %}checked="checked" {% endif %}/>
                    {{ option.option }}
                </label>
                {% if forloop.first %}
                    {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
                {% endif %}
            {% endwith %}
        {% endfor %}
    {% elseif blk.input_type == 'select' %}
        <select id="{{ #id }}" name="{{ blk.name }}">
        {% for option in props.answers %}
            <option value="{{ option.value }}" {% if option.value|member:ans %}selected="selected"{% endif %}>{{ option.option }}</option>
        {% endfor %}
        </select>
        {% if blk.is_required %}{% validate id=#id name=blk.name type={presence} %}{% endif %}
    {% elseif blk.input_type == 'submit' and not editing %}
        {% for option in props.answers %}
            {% with forloop.counter as index %}
                <button id="{{ #thur.index }}" name="{{ blk.name }}" value="{{ option.value }}" class="btn" type="submit">
                    <span></span>{{ option.option }}
                </button>
            {% endwith %}
        {% endfor %}
    {% else %}
        {% for option in props.answers %}
            {% with forloop.counter as index %}
                <label class="radio">
                    <input id="{{ #thur.index }}" name="{{ blk.name }}" type="radio" value="{{ option.value }}" {% if option.value|member:ans %}checked="checked" {% endif %}/>
                    {{ option.option }}
                </label>
                {% if forloop.first %}
                    {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
                {% endif %}
            {% endwith %}
        {% endfor %}
    {% endif %}
    </div>
    {% endwith %}
    {% endwith %}
{% endif %}

