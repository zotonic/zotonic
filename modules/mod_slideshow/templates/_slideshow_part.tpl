<li style="{% if slide_width %}width: {{ slide_width }}px;{% endif %}{% if slide_height %}height: {{ slide_height }}px; overflow:hidden;{% endif %}">
	{% with m.rsc[id].depiction as depiction %}
		{% if depiction %}
			<a href="{{ m.rsc[id].page_url }}">{% image depiction width=width|default:slide_width height=height|default:slide_height crop %}</a>
		{% endif %}
	{% endwith %}
</li>