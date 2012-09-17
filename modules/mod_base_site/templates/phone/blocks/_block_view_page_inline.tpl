{% if is_dialog %}
	{% with id.depiction as dep %}
	{% if dep and not dep.id.is_a.document %}
	<div class="thumbnail depiction landscape">
		<img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" />
		{% if dep.id.summary %}
		<p class="caption"><span class="icon icon-camera"></span> <a href="{{ dep.id.page_url }}">{{ dep.id.summary }}</a></p>
		{% endif %}
	</div>
	{% endif %}
	{% endwith %}
{% endif %}

<div class="block-text">
	{{ id.body|show_media }}

	{# only show simple blocks, we don't want any recursion in the block views #}

	{% for blk in id.blocks %}
		{% if blk.type == "header" %}
			{% include "blocks/_block_view_header.tpl" %}
		{% elseif blk.type == "text" %}
			{% include "blocks/_block_view_text.tpl" %}
		{% endif %}
	{% endfor %}
</div>
