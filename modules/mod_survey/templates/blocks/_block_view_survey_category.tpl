{% include "_survey_block_name_check.tpl" %}
{% with answers[blk.name]|survey_answer_split:blk as ans %}
{% with m.search[{all_bytitle cat=blk.category}] as list %}
<div class="control-group survey-category type-{{ blk.input_type|default:'single' }} question-{{ nr }}">
    <label class="control-label">{{ blk.prompt }}</label>
    <div class="controls">
{% if blk.explanation %}
        <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
{% if blk.input_type == 'multi' %}
    {% for title,val in list %}
        {% with forloop.counter as index %}
            <label class="checkbox">
                <input id="{{ #cat.index }}" name="{{ blk.name }}" type="checkbox" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ title }}
            </label>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#cat.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% elseif blk.input_type == 'submit' %}
    {% for title,val in list %}
        {% with forloop.counter as index %}
            <button id="{{ #cat.index }}" name="{{ blk.name }}" value="{{ val }}" class="btn" type="submit">
                <span></span>{{ title }}
            </button>
        {% endwith %}
    {% endfor %}
{% else %}
    {% for title,val in list %}
        {% with forloop.counter as index %}
            <label class="radio">
                <input id="{{ #cat.index }}" name="{{ blk.name }}" type="radio" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ title }}
            </label>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#cat.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% endif %}
    </div>
</div>
{% endwith %}
{% endwith %}
