{% if blk.style == 'quote' %}
	<blockquote>
		{{ blk.body|show_media }}
	</blockquote>
{% else %}
	<div class="block-text {% if blk.style == 'aside' %}block-aside{% endif %}">{{ blk.body|show_media }}</div>
{% endif %}
