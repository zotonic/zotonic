{% overrules %}

{% block content_group %}
    {% with qargs.qcontent_group as qcg_id %}
    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Content group _}</label>
        <div class="col-sm-9">
            <select class="form-control" name="qcontent_group">
                <option value=""></option>
                {% for cg in m.hierarchy.content_group.tree_flat %}
                    <option value="{{ cg.id }}" {% if qcg_id == cg.id %}selected{% endif %}>
                        {{ cg.indent }} {{ cg.id.title }}
                    </option>
                {% endfor %}
            </select>
        </div>
    </div>
    {% endwith %}
{% endblock %}
