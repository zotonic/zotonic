<div class="rsc-item" id="{{ #rsc_item }}">
{% block rsc_item %}
	{% if show_medium %}
	   	{% image id.medium mediaclass="admin-list-overview" class="thumb pull-left" %}
	{% else %}
	   	{% image id.depiction mediaclass="admin-list-overview" class="thumb pull-left" %}
	{% endif %}
	<strong>
        <a id="{{ #edit }}" href="{% url admin_edit_rsc id=id %}">
            {% block title %}{{ id.title|default:id.short_title|default:("<em>" ++ _"untitled" ++ "</em>") }}{% endblock %}
        </a>
    </strong><br />
    {% block meta %}
        <div class="text-muted">
            {{ id.category.id.title }}
        </div>
    {% endblock %}
{% endblock %}
</div>

{% if is_page_block %}
    {% wire
        id=#edit
        action={dialog_edit_basics id=id update_element=#rsc_item template="_rsc_item.tpl"}
    %}
{% endif %}
