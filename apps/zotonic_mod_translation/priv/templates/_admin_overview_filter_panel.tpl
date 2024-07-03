{% overrules %}

{% block properties %}
    <div class="form-group">
        <label class="col-sm-3 control-label">{_ With language _}</label>
        <div class="col-sm-9">
            <select name="qlanguage" class="form-control">
                <option value=""></option>
                {% for code, lang in m.translation.language_list_editable %}
                    <option value="{{ code }}" {% if qargs.qlanguage == code %}selected{% endif %}>
                        {{ lang.name }}  ({{ code }})
                    </option>
                {% endfor %}
            </select>
        </div>
    </div>

    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Without language _}</label>
        <div class="col-sm-9">
            <select name="qnotlanguage" class="form-control">
                <option value=""></option>
                {% for code, lang in m.translation.language_list_editable %}
                    <option value="{{ code }}" {% if qargs.qnotlanguage == code %}selected{% endif %}>
                        {{ lang.name }}  ({{ code }})
                    </option>
                {% endfor %}
            </select>
        </div>
    </div>
{% endblock %}
