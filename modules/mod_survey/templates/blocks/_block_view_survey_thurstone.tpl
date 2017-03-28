{% include "_survey_block_name_check.tpl" %}

{% if blk.is_test %}
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
    {% if blk.explanation %}
        <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
    {% endif %}
    </div>
    {% endwith %}
    {% endwith %}
{% endif %}
