{% include "_article_keywords.tpl" %}

{% with m.rsc[id].media as media %} 
	{% if media %}
		<ul class="images-list">
			{% for m in media %}
				<li>
					{% ifequal m.rsc[m].medium.mime "text/html-video-embed" %}
						<section class="video-wrapper">
							{% media m.rsc[m].medium %}
						</section>
					{% else %}
						<figure class="image-wrapper block-level-image">
							{% media m width=315 crop %}
							{% if m.rsc[m].summary %}<p class="image-caption">{{ m.rsc[m].summary }}</p>{% endif %}
						</figure>	
					{% endifequal %}
				</li>
			{% endfor %}
		</ul>
	{% endif %}	
{% endwith %}

{% if m.rsc[id].is_editable %}
	{% button text=_"edit this page" action={redirect dispatch="admin_edit_rsc" id=id} %}
{% endif %}