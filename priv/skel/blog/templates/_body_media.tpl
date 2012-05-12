{% ifequal align "block" %}
	{% ifequal m.rsc[id].medium.mime "text/html-video-embed" %}
		<section class="video-wrapper">
			{% media m.rsc[id].medium %}
		</section>
	{% else %}
		<figure class="image-wrapper block-level-image">
			{% media m.rsc[id].medium width=size.width|default:width height=size.height|default:height crop=crop class=align link=link alt=m.rsc[id].title %}
			{% with id|summary as summary %}{% if summary %}<p class="image-caption">{{ summary }}</p>{% endif %}{% endwith %}
		</figure>
	{% endifequal %}
{% else %}
	{% media m.rsc[id].medium width=size.width|default:width height=size.height|default:height crop=crop class=align link=link alt=m.rsc[id].title %}
{% endifequal %}
