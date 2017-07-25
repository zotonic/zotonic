{#
params:
- selected_value
- option_class
#}
{% for c in m.category.tree_flat %}
    {% if m.acl.insert[c.id.name|as_atom] %}
        <li class="{% if c.id.name == selected_value %}active{% endif %}">
            <a href="#" class="{{ option_class }}" data-value="{{ c.id.name }}">{{ c.indent }}{{ c.id.title|default:c.id.name }}</a>
        </li>
    {% endif %}
{% endfor %}
