{% if id.medium as medium %}
	<figure class="image-wrapper {% if align == 'block' %}block-level-image{% else %}pull-{{align}} inline-image{% endif %} category-{{ id.category_id.name }} {{ mediaclass }} {% if not medium.filename %}embed{% endif %}">
		{% if medium.mime == "text/html-oembed" %}
			<div class="oembed-wrapper">
				{% media medium mediaclass=mediaclass %}
			</div>
		{% else %}
			{% media medium mediaclass=mediaclass crop=crop link=link alt=id.title %}
			{% if caption /= '-' %}
				{% if caption|default:(id|summary) as caption %}
					<figcaption class="image-caption">{{ caption }}</figcaption>
				{% endif %}
			{% endif %}
		{% endif %}
	</figure>
{% endif %}
