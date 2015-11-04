<div class="rsc-item" id="{{ #item }}">
	{% if show_medium %}
	   	{% image id.medium mediaclass="admin-list-overview" class="thumb pull-left" %}
	{% else %}
	   	{% image id.depiction mediaclass="admin-list-overview" class="thumb pull-left" %}
	{% endif %}
	<strong><a href="{% url admin_edit_rsc id=id %}">{{ id.title }}</a></strong><br />
    <div class="text-muted">
        {{ id.category.id.title }}
    </div>
</div>
