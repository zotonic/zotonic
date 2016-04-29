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

{% with m.search[{query cat=`acl_collaboration_group`}] as collaboration_groups %}
    <li class="divider"></li>
    <li role="presentation" class="dropdown-header">
        {{ m.rsc.acl_collaboration_group.title }}
    </li>
    {% for id in collaboration_groups %}
        <li class="{% if id == selected_value %}active{% endif %}">
            <a href="#" class="{{ option_class }}" data-value="{{ id }}">{{ id.title }}</a>
        </li>
    {% endfor %}
{% endwith %}
