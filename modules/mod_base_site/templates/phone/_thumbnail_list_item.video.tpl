{% if id.is_visible and id.is_published %}
<li class="span12 thumbnail video">
    {% media id width=400 %}
	<p class="caption"><span class="icon icon-camera"></span> <a href="{{ id.page_url }}">{{ id.summary|default:id.title|truncate:60 }}</a></p>
</li>
{% endif %}
