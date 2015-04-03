{% if is_nocatselect %}

    <select class="form-control" id="{{ cgsel_id|default:#content_group_id }}" name="content_group_id">
        {% for cg in m.hierarchy.content_group.tree_flat %}
            {% if m.acl_rule.can_insert[cg.id][cat_id] %}
                <option value="{{ cg.id }}" 
                    {% if not content_group_id and cg.id.name == 'default_content_group'%}
                        selected
                    {% endif %}
                >
                    {{ cg.indent }}{{ cg.id.title }}
                </option>
            {% endif %}
        {% endfor %}
    </select>

{% elseif is_cg_reload %}

    <select class="form-control" id="{{ cgsel_id|default:#content_group_id }}" name="content_group_id">
        {% for cg in m.hierarchy.content_group.tree_flat %}
            {% if cg.id|member:cg_allowed %}
                <option value="{{ cg.id }}" 
                    {% if cg_id == cg.id or (not cg_id and cg.id.name == 'default_content_group') %}
                        selected
                    {% endif %}
                >
                    {{ cg.indent }}{{ cg.id.title }}
                </option>
            {% endif %}
        {% endfor %}
    </select>

{% else %}

    {% with cat_id|default:id.category_id as category_id %}
    {% with cg_id|default:id.content_group_id as content_group_id %}

        <select class="form-control" id="{{ cgsel_id|default:#content_group_id }}" name="content_group_id">
            {% if category_id %}
                {% for cg in m.hierarchy.content_group.tree_flat %}
                    {% if content_group_id == cg.id %}
                        <option value="{{ cg.id }}" selected>
                            {{ cg.indent }}{{ cg.id.title }}
                        </option>
                    {% elseif m.acl_rule.can_insert[cg.id][category_id] %}
                        <option value="{{ cg.id }}" 
                            {% if not content_group_id and cg.id.name == 'default_content_group'%}
                                selected
                            {% endif %}
                        >
                            {{ cg.indent }}{{ cg.id.title }}
                        </option>
                    {% endif %}
                {% endfor %}
            {% endif %}
        </select>

    {% endwith %}
    {% endwith %}
{% endif %}

{% validate id=cgsel_id|default:#content_group_id name="content_group_id" type={presence} %}
