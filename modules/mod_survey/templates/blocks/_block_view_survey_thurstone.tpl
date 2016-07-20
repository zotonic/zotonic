{% include "_survey_block_name_check.tpl" %}
{% with blk|survey_prepare_thurstone as props %}
{% with answers[blk.name]|survey_answer_split:blk as ans %}
<div class="form-group survey-thurstone type-{{ blk.input_type|default:'single' }} question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label">{{ blk.prompt }}</label>
{% if blk.explanation %}
    <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
{% if blk.input_type == 'multi' %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <div class="checkbox"><label>
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="checkbox" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ item }}
            </label></div>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% elseif blk.input_type == 'select' %}
    <select class="form-control" id="{{ #id }}" name="{{ blk.name }}">
    {% for val,item in props.answers %}
        <option value="{{ val }}" {% if val|member:ans %}selected="selected"{% endif %}>{{ item }}</option>
    {% endfor %}
    </select>
    {% if blk.is_required %}{% validate id=#id name=blk.name type={presence} %}{% endif %}
{% elseif blk.input_type == 'submit' %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <button id="{{ #thur.index }}" name="{{ blk.name }}" value="{{ val }}" class="btn btn-default" type="submit">
                <span></span>{{ item }}
            </button>
        {% endwith %}
    {% endfor %}
{% else %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <div class="radio"><label>
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="radio" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ item }}
            </label></div>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#thur.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% endif %}
</div>
{% endwith %}
{% endwith %}
