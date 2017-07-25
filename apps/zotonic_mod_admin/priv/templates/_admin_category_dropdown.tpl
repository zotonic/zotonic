{% with
    cat_id|default:m.rsc[id].category_id
    as
    r_cat
%}
<select id="category_id" name="category_id" class="col-lg-4 col-md-4 form-control">
    {% for c in m.category.tree_flat %}
	    {% if m.acl.insert[c.id.name|as_atom] %}
	    <option value="{{ c.id }}" {% if r_cat == c.id %}selected="selected"{% endif %}>
	        {{ c.indent }}{{ c.id.title|default:c.id.name }}
	    </option>
	    {% endif %}
    {% endfor %}
</select>
{% endwith %}
