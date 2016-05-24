<td>
    {% if rule.category_id %}
        {{ rule.category_id.title }}
    {% else %}
        <em>{_ All Categories _}</em>
    {% endif %}
</td>
