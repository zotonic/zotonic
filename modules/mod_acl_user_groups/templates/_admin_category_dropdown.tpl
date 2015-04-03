{% with cat_id|default:id.category_id as category_id %}
	<select id="{{ catsel_id|default:#catid }}" name="category_id" class="col-lg-4 col-md-4 form-control">
		{% for c in m.category.tree_flat %}
			{% if m.acl_rule.can_insert.none[c.id] or c.id == category_id %}
				<option value="{{ c.id }}" {% if category_id == c.id %}selected="selected"{% endif %}>
					{{ c.indent }}{{ c.id.title|default:c.id.name }}
				</option>
			{% endif %}
		{% endfor %}
	</select>
	{% validate id=catsel_id|default:#catid name="category_id" type={presence} %}
{% endwith %}
