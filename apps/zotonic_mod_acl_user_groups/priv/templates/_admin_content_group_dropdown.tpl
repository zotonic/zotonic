{% with m.acl_user_group.has_collaboration_groups as collabs %}
{% if is_nocatselect %}

    <select class="form-control" id="{{ cgsel_id|default:#content_group_id }}" name="content_group_id">
        {% if collabs and m.acl_rule.can_insert.acl_collaboration_group[cat_id] %}
            <optgroup label="{{ m.rsc.content_group.title }}">
        {% endif %}
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
        {% if collabs and m.acl_rule.can_insert.acl_collaboration_group[cat_id] %}
            </optgroup>
            <optgroup label="{{ m.rsc.acl_collaboration_group.title }}">
                {% for cid in collabs %}
                    <option value="{{ cid }}">
                        {{ cid.title }}
                    </option>
                {% endfor %}
            </opgroup>
        {% endif %}
    </select>

{% elseif is_cg_reload %}

    <select class="form-control" id="{{ cgsel_id|default:#content_group_id }}" name="content_group_id">
        {% if not cg_id %}
            <option value=""></option>
        {% endif %}
        {% if collabs or cg_id.is_a.acl_collaboration_group %}
            <optgroup label="{{ m.rsc.content_group.title }}">
        {% endif %}
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
        {% if collabs or cg_id.is_a.acl_collaboration_group %}
            </optgroup>
            <optgroup label="{{ m.rsc.acl_collaboration_group.title }}">
                {% if cg_id
                    and cg_id.is_a.acl_collaboration_group
                    and not cg_id|member:collabs
                %}
                    <option value="{{ cg_id }}" selected {% if not cid|member:cg_allowed %}disabled{% endif %}>
                        {{ cg_id.title }}
                    </option>
                {% endif %}
                {% for cid in collabs %}
                    {% if cid|member:cg_allowed %}
                        <option value="{{ cid }}" {% if cg_id == cid %}selected{% endif %}>
                            {{ cid.title }}
                        </option>
                    {% endif %}
                {% endfor %}
            </opgroup>
        {% endif %}
    </select>

{% else %}

    {% with cat_id|default:id.category_id as category_id %}
    {% with cg_id|default:id.content_group_id as content_group_id %}

        <select class="form-control" id="{{ cgsel_id|default:#content_group_id }}" name="content_group_id">
            {% if category_id %}
                {% if collabs or content_group_id.is_a.acl_collaboration_group %}
                    <optgroup label="{{ m.rsc.content_group.title }}">
                {% endif %}
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
                {% if collabs or content_group_id.is_a.acl_collaboration_group %}
                    </optgroup>
                    <optgroup label="{{ m.rsc.acl_collaboration_group.title }}">
                        {% if content_group_id
                            and content_group_id.is_a.acl_collaboration_group
                            and not content_group_id|member:collabs
                        %}
                            <option value="{{ content_group_id }}" selected>
                                {{ content_group_id.title }}
                            </option>
                        {% endif %}
                        {% for cid in collabs %}
                            <option value="{{ cid }}" {% if content_group_id == cid %}selected{% endif %}>
                                {{ cid.title }}
                            </option>
                        {% endfor %}
                    </opgroup>
                {% endif %}
            {% endif %}
        </select>

    {% endwith %}
    {% endwith %}

{% endif %}
{% endwith %}

{% validate id=cgsel_id|default:#content_group_id name="content_group_id" type={presence} %}

