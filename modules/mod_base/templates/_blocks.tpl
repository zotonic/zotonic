{% for blk in m.rsc[id].blocks %}
	{% if blk.type == 'header' %}
		<h2 class="block-header" {% if blk.name %}id="{{ blk.name }}"{% endif %}>{{ blk.header }}</h2>
	{% elseif blk.body %}
		<div class="block-text">{{ blk.body|show_media }}</div>
	{% endif %}
{% endfor %}
