{% with blk|survey_prepare_thurstone as props %}
{% with answers[blk.name] as ans %}
<div class="control-group survey-thurstone">
    <label>{{ blk.prompt }}</label>
    <div class="controls">
{% if blk.is_multiple %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <label class="checkbox">
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="checkbox" value="{{ val|escape }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ item|escape }}
            </label>
            {% if forloop.first %}
                {% if question.is_required %}{% validate id=#thur.index name=name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% else %}
    {% for val,item in props.answers %}
        {% with forloop.counter as index %}
            <label class="radio">
                <input id="{{ #thur.index }}" name="{{ blk.name }}" type="radio" value="{{ val|escape }}" {% if val|member:ans %}checked="checked" {% endif %}/>
                {{ item|escape }}
            </label>
            {% if forloop.first %}
                {% if question.is_required %}{% validate id=#thur.index name=name type={presence} %}{% endif %}
            {% endif %}
        {% endwith %}
    {% endfor %}
{% endif %}
    </div>
</div>
{% endwith %}
{% endwith %}
