<div class="thumbnail depiction landscape">
	<a href="{{ id.page_url }}"><img src="{% image_url id mediaclass="base-page-main" %}" alt="{{ id.title }}" /></a>
	{% if id.summary %}
	<p class="caption"><span class="icon icon-camera"></span> <a href="{{ id.page_url }}">{{ id.summary }}</a></p>
	{% endif %}
</div>

<div class="block-text">{{ id.body|show_media }}</div>
