{% if id.is_visible %}
<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %}">
    {% if id.depiction as dep %}
        {% image dep class="float-left" mediaclass="small-crop" crop=crop link=link alt=id.title %}
    {% else %}
        <span class="fa-solid fa-search"></span>
    {% endif %}
    <span class="list-item__title">{{ id.title|default:_"Untitled" }}</span>
    <small class="text-muted list-item__meta">{{ id.category_id.title }}</small>
    <div class="btn-group" role="group">
        <a href="{{ id.page_url }}" class="btn btn-sm btn-outline-secondary">
            {_ view _}
        </a>
        {% if m.acl.use.mod_admin and id.is_editable %}
            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-sm btn-outline-secondary">
                {_ edit _}
            </a>
        {% endif %}
    </div>
</div>
{% endif %}
