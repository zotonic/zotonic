{% with blk|survey_prepare_matching as props %}
<div class="control-group survey-matching">
    <label>{{ blk.prompt }}</label>
    <div class="controls">
    {% for val,item in props.items %}
        {% with forloop.counter as index %}
        {% with [name, "_", index|make_list]|join as nm %}
        {% with answers[nm] as ans %}
            <label for="{{ #match.index }}">{{ item|escape }}</label>
            <select id="{{ #match.index }}" name="{{ nm }}">
                <option></option>
                {% for v,opt in props.options %}
                    <option value="{{ v|escape }}" {% if v == ans %}selected="selected"{% endif %}>{{ opt|escape }}</option>
                {% endfor %}
            </select>
            {% if blk.is_required %}
                {% validate id=#match.index name=nm type={presence} %}
            {% endif %}
        {% endwith %}
        {% endwith %}
        {% endwith %}
    {% endfor %}
    </div>
</div>
{% endwith %}
