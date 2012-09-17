<div class="block-text block-inline">
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
