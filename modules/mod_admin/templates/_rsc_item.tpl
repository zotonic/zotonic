<div class="rsc-item" id="{{ #item }}">
    {% with id.depiction as depict %}
    	{% image depict mediaclass="admin-list-overview" class="thumb pull-left" %}
    {% endwith %}
	<strong><a href="#{{ id }}">{{ id.title }}</a></strong><br />
	<span class="text-muted">{{ id|summary:50 }}</span>
</div>
