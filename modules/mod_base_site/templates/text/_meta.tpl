<p class="meta">
    <span class="label">{{ id.category_id.title }}</span>
{% for id in id.o.author|is_visible %}
    {% if forloop.first %}{_ By _}{% endif %}
    <a href="{{ id.page_url }}">{{ id.title }}</a>{% if not forloop.last %}, {% else %} &ndash; {% endif %}
{% endfor %}
    {{ id.created|date:_"l, F j, Y"}}
    {% if id.is_editable %}
        <a class="btn btn-mini pull-right" href="{% url admin_edit_rsc id=id %}">{_ Edit _}</a>
    {% endif %}
</p>
