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
    {% if rule.visibility %}
        {{ rule.visibility }}
    {% else %}
        <em>{_ Any visibility _}</em>
    {% endif %}
</td>
