{% ifequal align "block" %}
	{% ifequal m.rsc[id].medium.mime "text/html-oembed" %}
		<section class="oembed-wrapper">
			{% media m.rsc[id].medium %}
		</section>
	{% else %}
		<figure class="image-wrapper block-level-image">
			{% media m.rsc[id].medium width=size.width|default:width height=size.height|default:height crop=crop class=align link=link alt=m.rsc[id].title %}
			{% if caption|default:(id|summary) as caption %}
				<figcaption class="image-caption">{{ caption }}</figcaption>
			{% endif %}
		</figure>
	{% endifequal %}
{% else %}
	{% media m.rsc[id].medium width=size.width|default:width height=size.height|default:height crop=crop class=align link=link alt=m.rsc[id].title %}
{% endifequal %}
