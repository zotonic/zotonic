{% with blk|survey_prepare_matching as props %}
    <div class="{% if blk.is_test_direct %}do_survey_test_feedback {% endif %}control-group survey-matching question-{{ nr }} {% if not blk.prompt %}noprompt{% endif %}">
        <label class="control-label">{{ blk.prompt }}</label>
    {% if blk.explanation %}
         <p class="help-block">{{ blk.explanation|linebreaksbr }}</p>
    {% endif %}
        <div class="controls">
        {% for val,item in props.items %}
            {% with forloop.counter as index %}
            {% with [blk.name, "_", val]|join|to_binary as nm %}
            {% with answers[nm] as ans %}
                <div>
                    <label for="{{ #match.index }}">{{ item|escape }}</label>
                    <select id="{{ #match.index }}" name="{{ nm }}"
                            {% if blk.is_test_direct %}data-is-correct="{{ val|escape }}"{% endif %}
                    >
                        <option></option>
                        {% for v,opt in props.options %}
                            <option value="{{ v|escape }}" {% if v == ans %}selected="selected"{% endif %}>{{ opt|escape }}</option>
                        {% endfor %}
                    </select>
                </div>
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
