{% if id.is_visible and id.is_published %}
<li class="span3">
	<a href="{{ d.page_url }}" class="thumbnail"><img src="{% image_url id mediaclass="base-thumbnail" %}" alt="{{ id.title }}" title="{{id.title}}"/></a>
</li>
{% endif %}
