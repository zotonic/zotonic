{#
params:
- selected_qvalue
- option_class
#}
{% for c in m.category.tree_flat %}
    {% if m.acl.insert[c.id.name|as_atom] %}
        <li class="{% if c.id.name == selected_qvalue %}active{% endif %}">
            <a href="#" class="dropdown-item {{ option_class }}" data-value="{{ c.id.name }}">{{ c.indent }}{{ c.id.title|default:c.id.name }}</a>
        </li>
    {% endif %}
{% endfor %}
