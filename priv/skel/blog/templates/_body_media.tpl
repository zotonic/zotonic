{% ifequal align "block" %}
	{% ifequal m.rsc[id].medium.mime "text/html-video-embed" %}
		<section class="video-wrapper">
			{% media m.rsc[id].medium %}
		</section>
	{% else %}
		{% ifequal m.rsc[id].medium.mime "audio/mpeg" %}
			{% include "_media_audiofile.tpl" id=m.rsc[id].medium.id %}
		{% else %}
			<figure class="image-wrapper block-level-image">
				{% media m.rsc[id].medium width=size.width height=size.height crop=crop class=align link=link alt=m.rsc[id].title %}
				{% if m.rsc[id].summary %}<p class="image-caption">{{ m.rsc[id].summary }}</p>{% endif %}
			</figure>	
		{% endifequal %}
	{% endifequal %}
{% else %}
	{% media m.rsc[id].medium width=size.width height=size.height crop=crop class=align link=link alt=m.rsc[id].title %}
{% endifequal %}
