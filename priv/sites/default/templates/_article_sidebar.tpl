{% include "_article_keywords.tpl" %}

{% with m.rsc[id].media as media %} 
	{% if media %}
		<ul class="images-list">
			{% for m in media %}
				<li>{% media m width=315 crop %}</li>
			{% endfor %}
		</ul>
	{% endif %}	
{% endwith %}

{% if m.rsc[id].is_editable %}
	{% button text=_"edit this page" action={redirect dispatch="admin_edit_rsc" id=id} %}
{% endif %}