{#
params:
- selected_value
- option_class
#}
{% for cat_id, level, indent, name in m.category.all_flat %}
    {% if m.acl.insert[name|as_atom] %}
        <li class="{% ifequal name selected_value %}active{% endifequal %}">
            <a href="#" class="{{ option_class }}" data-value="{{ name }}">{{ indent }}{{ m.rsc[cat_id].title|default:name }}</a>
        </li>
    {% endif %}
{% endfor %}