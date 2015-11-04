{#
params:
- selected_value
- option_class
#}
{% for name,p in m.predicate %}
    <li class="{% if p.id == selected_value %}active{% endif %}">
        <a href="#" class="{{ option_class }}" data-value="{{ p.id }}">{{ p.title|default:p.name }}</a>
    </li>
{% endfor %}
