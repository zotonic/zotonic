{% include "_survey_block_name_check.tpl" %}
{% with answers[blk.name]|survey_answer_split:blk as ans %}
{% with m.search[{all_bytitle cat=blk.category}] as list %}
<div class="form-group survey-category type-{{ blk.input_type|default:'single' }} question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}"|default:'single' }} question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
    <label class="control-label">{{ blk.prompt }}</label>
    <div>
{% if blk.explanation %}
        <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
{% endif %}
{% if blk.input_type == 'multi' %}
    {% for title,val in list %}
        {% with forloop.counter as index %}
            <div class="checkbox"><label>
                <input id="{{ #cat.index }}" name="{{ blk.name }}" type="checkbox" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ title }}
            </label></div>
            {% if forloop.first %}
                {% if blk.is_required %}{% validate id=#cat.index name=blk.name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% elseif blk.input_type == 'submit' %}
    {% for title,val in list %}
        {% with forloop.counter as index %}
            <button id="{{ #cat.index }}" name="{{ blk.name }}" value="{{ val }}" class="btn btn-default" type="submit">
                <span></span>{{ title }}
            </button>
        {% endwith %}
    {% endfor %}
{% else %}
    {% for title,val in list %}
        {% with forloop.counter as index %}
            <div class="radio"><label>
                <input id="{{ #cat.index }}" name="{{ blk.name }}" type="radio" value="{{ val }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ title }}
            </label></div>
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
