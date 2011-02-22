{% with m.rsc[id].depiction as depiction %}
	{% if depiction %}
		<a href="{{ m.rsc[id].page_url }}">{% image depiction width=width|default:slide_width height=height|default:slide_height crop %}</a>
	{% endif %}
{% endwith %}
