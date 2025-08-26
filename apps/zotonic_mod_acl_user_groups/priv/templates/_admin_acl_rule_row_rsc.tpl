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
    {% with rule.content_group_id as cgid %}
        {% if not cgid %}
            <em>{_ All normal content groups _}</em>
        {% elseif cgid == m.rsc.acl_collaboration_group.id %}
            <span class="z-icon z-icon-user"></span>
            {_ All collaboration groups _}
        {% elseif cgid.is_a.acl_collaboration_group %}
            <span class="z-icon z-icon-user"></span>
            {{ cgid.title }}
        {% else %}
            {% for cg in m.hierarchy.content_group.tree_flat %}
                {% if cg.id == cgid %}
                    {{ cg.indent }} {{ cgid.title }}
                {% endif %}
            {% endfor %}
        {% endif %}
    {% endwith %}
</td>
<td>
    {% if rule.category_id %}
        {% if rule.is_category_exact %}
            =
        {% endif %}

        {{ rule.category_id.title }}
    {% else %}
        <em>{_ All Categories _}</em>
    {% endif %}
</td>
<td>
    {% if rule.visibility|is_undefined %}
        <em>{_ Any visibility _}</em>
    {% elif rule.visibility == 0 %}
        0 <em>{_ (public) _}</em>
    {% elif rule.visibility == 50 %}
        50 <em>{_ (private) _}</em>
    {% else %}
        {{ rule.visibility }}
    {% endif %}
</td>
