{% ifequal align "block" %}
	{% ifequal m.rsc[id].medium.mime "text/html-oembed" %}
		<section class="oembed-wrapper">
			{% media m.rsc[id].medium %}
		</section>
	{% else %}
		<figure class="image-wrapper block-level-image">
			{% media m.rsc[id].medium width=size.width height=size.height crop=crop class=align link=link alt=m.rsc[id].title %}
			{% if caption|default:m.rsc[id].summary as caption %}
				<figcaption class="image-caption">{{ caption }}</figcaption>
			{% endif %}
		</figure>
	{% endifequal %}
{% else %}
	{% media m.rsc[id].medium width=size.width height=size.height crop=crop class=align link=link alt=caption|default:m.rsc[id].title %}
{% endifequal %}
