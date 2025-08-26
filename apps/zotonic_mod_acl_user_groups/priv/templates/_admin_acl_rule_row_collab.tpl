<td>
    {% if rule.category_id %}
        {% if rule.is_category_exact %}
            =
        {% endif %}
        {{ rule.category_id.title }}
    {% else %}
        <em>{_ All Categories _}</em>
    {% endif %}
</td>
<td>
    {% if rule.visibility|is_undefined %}
        <em>{_ Any visibility _}</em>
    {% elif rule.visibility == 0 %}
        0 <em>{_ (public) _}</em>
    {% elif rule.visibility == 50 %}
        50 <em>{_ (private) _}</em>
    {% else %}
        {{ rule.visibility }}
    {% endif %}
</td>
