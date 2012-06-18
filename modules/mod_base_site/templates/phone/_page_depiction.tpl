{# page depiction for phone #}
{% with id.depiction as dep %}
{% if dep and not dep.id.is_a.document %}
<div class="thumbnail depiction landscape">
	<a href="{{ dep.id.page_url }}"><img src="{% image_url dep mediaclass="base-page-main" %}" alt="{{ dep.id.title }}" /></a>
	{% if dep.id.summary %}
	<p class="caption"><span class="icon icon-camera"></span> <a href="{{ dep.id.page_url }}">{{ dep.id.summary }}</a></p>
	{% endif %}
</div>
{% endif %}
{% endwith %}
