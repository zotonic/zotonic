{% if id.is_visible and id.is_published %}
<li class="thumbnail video col-lg-12 col-md-12">
    {% media id width=400 %}
	<p class="caption"><span class="icon glyphicon glyphicon-camera"></span> <a href="{{ id.page_url }}">{{ id.summary|default:id.title|truncate:60 }}</a></p>
</li>
{% endif %}
