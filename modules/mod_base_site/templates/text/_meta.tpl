<p class="meta">
    <a href="{{ id.category_id.page_url }}"><span class="label">{{ id.category_id.title }}</span></a>
{% for id in id.o.author|is_visible %}
    {% if forloop.first %}{_ By _}{% endif %}
    <a href="{{ id.page_url }}">{{ id.title }}</a>{% if not forloop.last %}, {% else %} &ndash; {% endif %}
{% endfor %}
    {{ id.created|date:_"l, F j, Y"}}
    {% if id.is_editable %}
        <a class="btn btn-mini pull-right" href="{% url admin_edit_rsc id=id %}">{_ Edit _}</a>
    {% endif %}
    {% all include "_meta_extra.tpl" %}
</p>
