{% with blk|survey_prepare_thurstone as props %}
{% with answers[blk.name]|survey_answer_split:blk as ans %}
<div class="{% if blk.is_test_direct %}do_survey_test_feedback {% endif %}form-group survey-thurstone type-{{ blk.input_type|default:'single' }} question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label">{{ blk.prompt }}</label>
    <div class="controls">

{% if blk.input_type == 'multi' %}

    {% for option in props.answers %}
        {% with forloop.counter as index %}
            <label class="checkbox">
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="checkbox"
                       value="{{ option.value }}" {% if option.value|member:ans %}checked="checked" {% endif %}
                       {% if blk.is_test_direct %}data-is-correct="{{ option.is_correct }}"{% endif %}
                       data-answer-nr="{{ index }}"
                >
                {{ option.option }}
            </label>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
            {% endif %}

            {% if blk.is_test_direct %}
                {% if option.feedback %}
                    {% if option.is_correct %}
                        <p class="survey-test-feedback-answer text-success" data-answer-nr="{{ index }}">
                            {{ option.feedback }}
                        </p>
                    {% else %}
                        <p class="survey-test-feedback-answer text-error" data-answer-nr="{{ index }}">
                            {{ option.feedback }}
                        </p>
                    {% endif %}
                {% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}

{% elseif blk.input_type == 'select' %}

    <select id="{{ #id }}" name="{{ blk.name }}">
        {% if blk.is_test %}
            <option></option>
        {% endif %}
    {% for option in props.answers %}
        <option value="{{ option.value }}"
                {% if option.value|member:ans %}selected="selected"{% endif %}
                {% if blk.is_test_direct %}data-is-correct="{{ option.is_correct }}"{% endif %}
                data-answer-nr="{{ forloop.counter }}"
        >{{ option.option }}</option>
    {% endfor %}
    </select>
    {% if blk.is_required %}
        {% validate id=#id name=blk.name type={presence} %}
    {% endif %}

    {% if blk.is_test_direct %}
        {% for option in props.answers %}
            {% if option.feedback %}
                {% if option.is_correct %}
                    <p class="survey-test-feedback-answer text-success" data-answer-nr="{{ forloop.counter }}">
                        {{ option.feedback }}
                    </p>
                {% else %}
                    <p class="survey-test-feedback-answer text-error" data-answer-nr="{{ forloop.counter }}">
                        {{ option.feedback }}
                    </p>
                {% endif %}
            {% endif %}
        {% endfor %}
    {% endif %}

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
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="radio" value="{{ option.value }}"
                    {% if option.value|member:ans %}checked="checked" {% endif %}
                    {% if blk.is_test_direct %}data-is-correct="{{ option.is_correct }}"{% endif %}
                    data-answer-nr="{{ index }}"
                    >
                {{ option.option }}
            </label>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
            {% endif %}

            {% if blk.is_test_direct %}
                {% if option.feedback %}
                    {% if option.is_correct %}
                        <p class="survey-test-feedback-answer text-success" data-answer-nr="{{ index }}">
                            {{ option.feedback }}
                        </p>
                    {% else %}
                        <p class="survey-test-feedback-answer text-error" data-answer-nr="{{ index }}">
                            {{ option.feedback }}
                        </p>
                    {% endif %}
                {% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}

{% endif %}
    </div>

{% if blk.is_test_direct %}
    {% if blk.test_correct %}
        <p class="survey-test-feedback-correct text-success" data-answer-nr="0">
            &#10140; {{ blk.test_correct }}
        </p>
    {% endif %}
    {% if blk.test_wrong %}
        <p class="survey-test-feedback-wrong text-error" data-answer-nr="0">
            &#10140; {{ blk.test_wrong }}
        </p>
    {% endif %}
{% endif %}

{% if blk.explanation %}
    <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
</div>
{% endwith %}
{% endwith %}
