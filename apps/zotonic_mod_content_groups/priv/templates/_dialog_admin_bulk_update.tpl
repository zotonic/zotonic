{% overrules %}

{% block content_group %}
    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Content group _}</label>
        <div class="col-sm-9">
            <select class="form-control" name="content_group_id">
                <option value=""></option>
                {% for cg in m.hierarchy.content_group.tree_flat %}
                    <option value="{{ cg.id }}">
                        {{ cg.indent }} {{ cg.id.title }}
                    </option>
                {% endfor %}
            </select>
        </div>
    </div>
{% endblock %}
