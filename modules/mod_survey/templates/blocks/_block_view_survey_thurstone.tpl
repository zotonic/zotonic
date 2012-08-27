{% with blk|survey_prepare_thurstone as props %}
{% with answers[blk.name]|survey_answer_split:blk as ans %}
<div class="control-group survey-thurstone type-{{ blk.input_type|default:'single' }} question-{{ nr }}">
    <label>{{ blk.prompt }}</label>
    <div class="controls">
{% if blk.explanation %}
        <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
{% if blk.input_type == 'multi' %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <label class="checkbox">
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="checkbox" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ item }}
            </label>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% elseif blk.input_type == 'submit' %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <button id="{{ #thur.index }}" name="{{ blk.name }}" value="{{ val }}" class="btn" type="submit">
                <span></span>{{ item }}
            </button>
        {% endwith %}
    {% endfor %}
{% else %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <label class="radio">
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="radio" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ item }}
            </label>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% endif %}
    </div>
</div>
{% endwith %}
{% endwith %}
