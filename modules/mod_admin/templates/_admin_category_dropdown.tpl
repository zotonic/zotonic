{% with
    cat_id|default:m.rsc[id].category_id
    as
    r_cat
%}
<select id="category_id" name="category_id" class="col-lg-4 col-md-4 form-control" required>
    <option value="" disabled {% if not r_cat %}selected{% endif %}>{_ Select category _}</option>
    {% for c in m.category.tree_flat %}
	    {% if m.acl.insert[c.id.name|as_atom] %}
	    <option value="{{ c.id }}" {% if r_cat == c.id %}selected="selected"{% endif %}>
	        {{ c.indent }}{{ c.id.title|default:c.id.name }}
	    </option>
	    {% endif %}
    {% endfor %}
</select>
{% validate id="category_id" type={presence} only_on_submit %}
{% endwith %}
