<div class="rsc-item" id="{{ #item }}">
	{% image id.medium mediaclass="admin-list-overview" class="thumb pull-left" %}
	<strong><a href="#{{ id }}">{{ id.title }}</a></strong><br />
	<span class="text-muted">{{ id|summary:50 }}</span>
</div>
