<li class="span3">
	<div class="thumbnail" data-id="{{ id }}">
		{% image id.medium mediaclass="admin-list-overview" class="thumb pull-left" %}
		<h5>{{ id.title }}</h5>
		<p>{{ id|summary:50 }}</p>
	</div>
</li>
