{% with cat_id|default:id.category_id as category_id %}
  	<select id="{{ catsel_id|default:#catid }}" name="category_id" class="form-control" required>
        <option value="" disabled {% if not category_id %}selected{% endif %}>{_ Select category _}</option>
        {% for c in m.category.tree_flat_meta %}
            {% if ( c.id == category_id
                    or (
                        m.acl_rule.can_insert.none[c.id]
                        and not c.id.name|member:[
                            'predicate',
                            'acl_user_group',
                            'content_group'
                        ]
                    )
                  )
                  and ( not cat_restrict
                        or m.category[c.id].is_a[cat_restrict])
                  and ( not subject_id
                        or predicate|is_undefined
                        or m.predicate.is_valid_object_category[predicate][c.id])
                  and ( not object_id
                        or predicate|is_undefined
                        or m.predicate.is_valid_subject_category[predicate][c.id])
            %}
                <option value="{{ c.id }}" {% if category_id == c.id %}selected="selected"{% endif %}>
                    {{ c.indent }}{{ c.id.title|default:c.id.name }}
                </option>
            {% endif %}
        {% endfor %}
    </select>
    {% validate id=catsel_id|default:#catid name="category_id" type={presence} only_on_submit %}
{% endwith %}
