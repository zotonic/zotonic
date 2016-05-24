<td>
    {% if rule.acl_user_group_id %}
        {% for cg in m.hierarchy.acl_user_group.tree_flat %}
            {% if cg.id == rule.acl_user_group_id %}
                {{ cg.indent }} {{ cg.id.title }}
            {% endif %}
        {% endfor %}
    {% else %}
        <em>{_ All User Groups _}</em>
    {% endif %}
</td>
<td>
    {% if rule.module %}
        <span class="text-muted">{{ rule.module }}</span>
        {{ m.modules.info[rule.module].title }}
    {% else %}
        <em>{_ All Modules _}</em>
    {% endif %}
</td>
