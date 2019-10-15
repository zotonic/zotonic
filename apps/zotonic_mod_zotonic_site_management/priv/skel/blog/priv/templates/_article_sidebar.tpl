{% include "_article_keywords.tpl" %}

{% with m.rsc[id].media|without_embedded_media:id as media %}
{% if media %}
    <ul class="list-unstyled">
	{% for medium in media %}
	<li>
		<a href="{{ medium.id.page_url }}" class="thumbnail">
			{% include "_body_media.tpl" width=360 align="block" id=medium %}
		</a>
	</li>
	{% endfor %}
</ul>
{% endif %}
{% endwith %}

{% include "_edit_button.tpl" %}
