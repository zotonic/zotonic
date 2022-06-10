{% if id.is_visible %}
<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %} do_clickable">
    {% if id.depiction as dep %}
        {% image dep class="float-left" mediaclass="small-crop" crop=crop link=link alt=id.title %}
    {% else %}
        <span class="glyphicon glyphicon-search"></span>
    {% endif %}
    <span>{{ id.title|default:_"Untitled" }}</span>
    <em>
        &ndash; <small class="text-muted"> {{ id.category_id.title }} </small>
    </em>
    <div class="btn-group float-right" role="group">
        <a href="{{ id.page_url }}" class="btn btn-default btn-secondary">
            {_ view _}
        </a>
        {% if m.acl.is_admin %}
            <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-secondary">
                {_ edit _}
            </a>
        {% endif %}
    </div>
</div>
{% endif %}
