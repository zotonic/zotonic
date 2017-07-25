<p class="text-muted">
    {% for id in id.o.author|is_visible %}
        {% if forloop.first %}{_ By _}{% endif %}
        <a href="{{ id.page_url }}">{{ id.title }}</a>{% if not forloop.last %}, {% else %} &ndash; {% endif %}
    {% endfor %}
    {{ id.created|date:_"l, F j, Y"}}
    <a href="{{ id.category_id.page_url }}" class="btn btn-default btn-xs">{{ id.category_id.title }}</a>
    {% all include "_meta_extra.tpl" %}
    <span class="pull-right">
        {% if id.is_editable %}
            <a class="btn btn-default btn-xs" href="{% url admin_edit_rsc id=id %}">{_ Edit _}</a>
        {% endif %}
        {% include "_meta_share.tpl" %}
    </span>
</p>
<hr />
