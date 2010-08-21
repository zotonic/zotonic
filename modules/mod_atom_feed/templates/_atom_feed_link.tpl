{% if cat and m.rsc[cat] %}
	<link rel="alternate" type="application/atom+xml" href="{% url atom_feed cat=m.rsc[cat].name %}" />
{% endif %}
