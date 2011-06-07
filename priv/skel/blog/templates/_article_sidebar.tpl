{% include "_article_keywords.tpl" %}

{% with m.rsc[id].media|without_embedded_media:id as media %} 
	{% if media %}
		<ul class="images-list">
			{% for medium in media %}
				<li>
                    {% include "_body_media.tpl" width=315 align="block" id=medium %}
				</li>
			{% endfor %}
		</ul>
	{% endif %}	
{% endwith %}

{% if m.rsc[id].is_editable %}
	{% button text=_"edit this page" action={redirect dispatch="admin_edit_rsc" id=id} %}
{% endif %}
