{% if is_editable %}
    <div class="col-md-12">
        <select class="form-control" id="{{ #category_id }}" name="category_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
            <option value="">{_ All _}</option>
            {% for cg in m.hierarchy[`$category`].tree_flat %}
                <option value="{{ cg.id }}" {% if cg.id == rule.category_id %}selected{% endif %}>
                    {{ cg.indent }} {{ cg.id.title }}
                </option>
            {% endfor %}
        </select>
    </div>
{% else %}
    <div class="col-md-2">
        {% if rule.category_id %}
            {{ rule.category_id.title }}
        {% else %}
            {_ All Categories _}
        {% endif %}
    </div>
{% endif %}
