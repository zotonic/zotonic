{% ifequal align "left" %}
	{% media m.rsc[id].medium width=203 height=203 crop class=align alt=m.rsc[id].title %}
{% endifequal %}

{% ifequal align "right" %}
	{% media m.rsc[id].medium width=203 height=203 crop class=align alt=m.rsc[id].title %}
{% endifequal %}

{% ifequal align "block" %}
	{% ifequal m.rsc[id].medium.mime "text/html-video-embed" %}
		<section class="video-wrapper">
			{% media m.rsc[id].medium %}
		</section>
	{% else %}
		<figure class="image-wrapper block-level-image">
			{% media m.rsc[id].medium width=445 crop class=align alt=m.rsc[id].title %}
			{% if m.rsc[id].summary %}<p class="image-caption">{{ m.rsc[id].summary }}</p>{% endif %}
		</figure>	
	{% endifequal %}
{% endifequal %}