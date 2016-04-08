{% if is_editable %}
    <div class="col-md-4">
        <select class="form-control" id="{{ #user_group_id }}" name="acl_user_group_id" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
            {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                <option value="{{ cg.id }}" {% if cg.id == rule.acl_user_group_id %}selected{% endif %}>
                    {{ cg.indent }} {{ cg.id.title }}
                </option>
            {% endfor %}
        </select>
    </div>
    <div class="col-md-8">
        <select class="form-control" id="{{ #module }}" name="module" {% if rule %}onchange="$(this.form).submit()"{% endif %}>
            <option value="">{_ All _}</option>
            {% for mod, name in m.modules.all %}
                <option value="{{ mod }}" {% if mod == rule.module %}selected{% endif %}>{{ mod }} ({{name }})</option>
            {% endfor %}
        </select>
    </div>
{% else %}
    <div class="col-md-2">
        {% if rule.acl_user_group_id %}
            {% for cg in m.hierarchy.acl_user_group.tree_flat %}
                {% if cg.id == rule.acl_user_group_id %}
                    {{ cg.indent }} {{ cg.id.title }}
                {% endif %}
            {% endfor %}
        {% else %}
            {_ All User Groups _}
        {% endif %}
    </div>
    <div class="col-md-4">
        {% if rule.module %}
            <span class="text-muted">{{ rule.module }}</span> {{ m.modules.info[rule.module].title }}
        {% else %}
            {_ All Modules _}
        {% endif %}
    </div>
{% endif %}
