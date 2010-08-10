<li>
	{% with m.rsc[id].depiction as depiction %}
		{% if depiction %}
			<a href="{{ m.rsc[id].page_url }}">{% image depiction width=width height=height crop %}</a>
		{% endif %}
	{% endwith %}
</li>