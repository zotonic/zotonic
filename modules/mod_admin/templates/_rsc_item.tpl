<div class="rsc-item" id="{{ #item }}">
	{% image id.medium mediaclass="admin-list-overview" class="thumb pull-left" %}
	<h5><a href="#{{ id }}">{{ id.title }}</a></h5>
	<p>{{ id|summary:50 }}</p>
</div>
