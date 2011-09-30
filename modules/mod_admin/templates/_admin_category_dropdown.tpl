{% with m.rsc[id].category_id as r_cat %}
<select id="category_id" name="category_id">
    {% for cat_id, level, indent, name in m.category.all_flat %}
    {% if m.acl.insert[name|as_atom] %}
    <option value="{{cat_id}}" {% ifequal r_cat cat_id %}selected="selected"{% endifequal %}>
        {{ indent }}{{ m.rsc[cat_id].title|default:name }}
    </option>
    {% endif %}
    {% endfor %}
</select>
{% endwith %}
