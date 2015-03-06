{#
    params:
    - selected_value
    - option_class
#}
{% for c in m.hierarchy.content_group.tree_flat %}
    <li class="{% if c.id == selected_value %}active{% endif %}">
        <a href="#" class="{{ option_class }}" data-value="{{ c.id }}">{{ c.indent }}{{ c.id.title }}</a>
    </li>
{% endfor %}
